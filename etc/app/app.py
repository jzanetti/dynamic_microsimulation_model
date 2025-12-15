import os
import pandas as pd
import dash
from dash import dcc, html, Input, Output, State, ctx
import dash_bootstrap_components as dbc
import plotly.graph_objects as go
from os.path import join

from process.Python.model.random_utlity_function import utility_func
from process.Python.model.validation import run_ruf_validation, run_ruf_sensitivity
from process.Python.data.tawa import tawa_data_preprocess
from process.Python.data.filename import create_hash_filename

HH_SIZE_PRESETS = {
    "No Filter": None,
    "Single Adult (No Kids)": {
        "H_Counts_Adults": [1, 1], 
        "H_Counts_DependentKids": [0, 0]
    },
    "Single Parent (1+ Kids)": {
        "H_Counts_Adults": [1, 1], 
        "H_Counts_DependentKids": [1, 10]
    },
    "Couple (No Kids)": {
        "H_Counts_Adults": [2, 2], 
        "H_Counts_DependentKids": [0, 0]
    },
    "Couple (1+ Kids)": { # This matches your commented example
        "H_Counts_Adults": [2, 2], 
        "H_Counts_DependentKids": [1, 3]
    }
}


def load_and_process_model(input_params, force_rerun=False, hes_path="etc/app/Synthetic-HES23-single-period.csv", output_dir="etc/app"):
    """
    Wraps the user's original main execution block.
    """
    filename_hash = create_hash_filename(input_params)
    if force_rerun or (not os.path.exists(f"{output_dir}/sensitivity_tests_{filename_hash}.csv")):
        # 1. Read Data
        hes_data = pd.read_csv(hes_path)

        # 2. Preprocess
        data = tawa_data_preprocess(
            hes_data,
            min_hourly_wage=input_params["min_hourly_wage"],
            hours_options=input_params["hours_options"],
            exclude_seniors=input_params["exclude_seniors"],
            apply_household_income_filter=input_params["apply_household_income_filter"],
            apply_earner_type_filter=input_params["apply_earner_type_filter"],
            apply_household_size_filter=input_params["apply_household_size_filter"]
        )

        # 3. Run Utility Function
        utility_func(
            data,
            input_params,
            income_name={"market": "market_income_per_hour"},
            working_hours_name="working_hours",
            output_dir=output_dir,
            recreate_data=True
        )

        # 4. Validation & Sensitivity
        run_ruf_validation(input_params, output_dir=output_dir)
        run_ruf_sensitivity(input_params, output_dir=output_dir)

    # 5. Retrieve Results for Plotting
    filename_hash = create_hash_filename(input_params)
    
    sens_path = join(output_dir, f"sensitivity_tests_{filename_hash}.csv")
    acc_path = join(output_dir, f"validation_score_{filename_hash}.csv")

    try:
        sensitivity_df = pd.read_csv(sens_path)
        accuracy_df = pd.read_csv(acc_path)
        
        # Extract scalar scores
        highest_utility_accuracy = accuracy_df[accuracy_df["scores"] == "highest_utility_accuracy"]["value"].values[0]
        total_hrs_accuracy = accuracy_df[accuracy_df["scores"] == "total_hrs_accuracy"]["value"].values[0]
        
        score_summary = f"Util Acc: {round(highest_utility_accuracy, 2)}% | Hrs Acc: {round(total_hrs_accuracy, 2)}%"

        return {
            "sensitivity": sensitivity_df.to_dict('records'),
            "metrics": {
                "utility_acc": highest_utility_accuracy, 
                "hours_acc": total_hrs_accuracy
            },
            "score_str": score_summary,
            "params": input_params,
            "success": True
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


# --- DASH APP SETUP ---

app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])
server = app.server


# 1. Define a helper function to create labels with help text
def render_label_with_help(label_text, help_text, id_suffix):
    return html.Div([
        html.Label(label_text, className="me-2"),  # The label itself
        
        # The 'Icon' (using a Badge for simplicity, no external icons needed)
        dbc.Badge(
            "?", 
            id=f"tooltip-target-{id_suffix}",
            color="light",
            text_color="primary",
            className="ms-1 border",
            style={"cursor": "pointer", "borderRadius": "50%", "padding": "4px 8px"}
        ),
        
        # The Tooltip component linking to the Badge
        dbc.Tooltip(
            help_text,
            target=f"tooltip-target-{id_suffix}",
            placement="right",  # Tooltip appears to the right of the sidebar
        ),
    ], className="d-flex align-items-center mb-1") # Flexbox to align text and icon

# Define Layout
app.layout = dbc.Container([
    dcc.Store(id='history-store', data=[]), # Stores list of previous runs

    html.H2("Employment Behaviour Model TestBed", className="my-4 text-center"),
    html.P(
            "This portal is used to test the Employment Behaviour Model ~ Cached runs are instant, but new scenarios will require calculation time", 
            className="text-center lead text-muted mb-4" # 'lead' makes it larger, 'text-muted' makes it grey
        ),
    # 2. Academic Context (Use dcc.Markdown for links, lighter text)
    dcc.Markdown(
        "The model is implemented based on the discrete choice model described in "
        "[Aaberge and Colombino (2018)](https://www.microsimulation.pub/articles/00177) and "
        "[Bronka et al. (2025)](https://www.microsimulation.pub/articles/00318).",
        className="text-center text-muted small",
        link_target="_blank" # Opens links in new tab
    ),
    dbc.Row([
        # --- LEFT PANEL: PARAMETERS ---
        dbc.Col([
            dbc.Card([
                dbc.CardHeader("Model Parameters"),
                dbc.CardBody([
                    # Wage
                    render_label_with_help(
                        "Min Hourly Wage ($)", 
                        "The minimum wage is assigned to people who just start working, " + \
                        " ideally this should come from Heckman Wage Model, but minimum wage is used here for simplicity", 
                        "wage"
                    ),
                    dbc.Input(id='input-wage', type='number', value=23.0, step=0.5, className="mb-2"),
                    
                    # Leisure
                    render_label_with_help(
                        "Leisure Value ($)", 
                        "How much leisure worth ($), by default it is set to minimum wage/hour", 
                        "leisure"
                    ),
                    dbc.Input(id='input-leisure', type='number', value=23.0, step=0.5, className="mb-2"),
                    
                    render_label_with_help(
                        "Working hours options (comma separated)", 
                        "How many hours people may choose to work per week. The result is very sensitive to this option.", 
                        "hours"
                    ),
                    dbc.Input(id='input-hours-opt', type='text', value="0, 10, 20, 30, 40", className="mb-2"),

                    render_label_with_help(
                        "Total Hours Constraint", 
                        "How many hours people can spend in total per week. Based on most literatures, it is set to 80 hours.", 
                        "hours2"
                    ),
                    dbc.Input(id='input-total-hours', type='number', value=80.0, className="mb-2"),

                    render_label_with_help(
                        "Household Income Filter (Min - Max)", 
                        "These parameters define thresholds to exclude values that are too large or too small, thereby preserving numerical stability.", 
                        "value_filter"
                    ),
                    dbc.Row([
                        dbc.Col(dbc.Input(id='input-inc-min', type='number', value=0.1, step=0.05)),
                        dbc.Col(dbc.Input(id='input-inc-max', type='number', value=0.7, step=0.05)),
                    ], className="mb-2"),

                    html.Hr(),
                    html.Label("Earner type:"),
                    dcc.Dropdown(
                        id='input-earner-type',
                        options=["All", "Primary", "Secondary"],
                        value="All",  # Default value
                        clearable=False,
                        className="mb-3"
                    ),


                    # Household filter
                    html.Label("Household Size/Type Filter"),
                    dcc.Dropdown(
                        id='input-hh-size',
                        options=[{'label': k, 'value': k} for k in HH_SIZE_PRESETS.keys()],
                        value="No Filter",  # Default value
                        clearable=False,
                        className="mb-3"
                    ),

                    dbc.Checklist(
                        options=[{"label": "Exclude Seniors (Seniors may have very different behaviours)", "value": True}],
                        value=[True],
                        id="input-exclude-seniors",
                        switch=True,
                        className="mb-3"
                    ),

                    html.Hr(),
                    dbc.Switch(
                        id="switch-force-rerun",
                        label="Force Rerun (Ignore Cache)",
                        value=False,
                        className="mb-3 text-danger" # Added red text to indicate it's 'heavy'
                    ),
                    # Buttons
                    dbc.Row([
                        dbc.Col(dbc.Button("Run Model", id="btn-run", color="primary", className="w-100"), width=8),
                        dbc.Col(dbc.Button("Refresh / Clear", id="btn-clear", color="secondary", outline=True, className="w-100"), width=4),
                    ]),
                    
                    dbc.Spinner(
                        html.Div(id="status-msg", className="mt-2 text-info", style={"fontSize": "0.9em"}),
                        fullscreen=True,  # This creates the "Pop Up" overlay effect
                        color="primary",  # Color of the spinner
                        type="border",    # Spinner style: "border" (circle) or "grow" (dots)
                        fullscreen_style={"opacity": "0.5"} # Optional: dims the background
                    )
                    # html.Div(id="status-msg", className="mt-2 text-info", style={"fontSize": "0.9em"})
                ])
            ], className="h-100")
        ], width=4),

        # --- RIGHT PANEL: VISUALIZATION ---
        dbc.Col([
            dbc.Card([
                dbc.CardHeader([
                    html.Span("Visualization Results"),
                    html.Span(id="header-scores", className="float-end text-success", style={"fontSize": "0.9rem", "fontWeight": "bold"})
                ]),

                dbc.CardBody([
                    dcc.Graph(id='graph-total-hours', style={"height": "400px"}),
                    html.Hr(),
                    html.H5("Run History & Accuracy Scores", className="mt-4"),
                    html.Div(id="score-table-container")
                ])
            ])
        ], width=8)
    ])
], fluid=True)


# --- CALLBACKS ---

@app.callback(
    [Output('history-store', 'data'),
     Output('status-msg', 'children')],
    [Input('btn-run', 'n_clicks'),
     Input('btn-clear', 'n_clicks')],
    [State('history-store', 'data'),
     State('input-wage', 'value'),
     State('input-leisure', 'value'),
     State('input-hours-opt', 'value'),
     State('input-total-hours', 'value'),
     State('input-inc-min', 'value'),
     State('input-inc-max', 'value'),
     State('input-exclude-seniors', 'value'),
     State("input-earner-type", "value"),
     State('input-hh-size', 'value'),
     State('switch-force-rerun', 'value')]
)
def update_model_and_store(
    n_run, n_clear, current_history, wage, leisure, hours_opt_str, tot_hours, inc_min, inc_max, exclude_seniors, earner_type, hh_size_selection, force_rerun_val):
    trigger_id = ctx.triggered_id

    # 1. Handle Clear/Refresh
    if trigger_id == 'btn-clear':
        return [], "Plots cleared."

    # 2. Handle Initial Load or No Click
    if not n_run:
        return current_history, "Ready."

    # 3. Handle Run
    try:
        # Parse inputs
        hours_list = [int(x.strip()) for x in hours_opt_str.split(',')]
        is_exclude_seniors = True if exclude_seniors and True in exclude_seniors else False
        selected_hh_filter = HH_SIZE_PRESETS.get(hh_size_selection)

        if earner_type == "All":
            earner_type = None

        input_params = {
            "total_hours": float(tot_hours),
            "min_hourly_wage": float(wage),
            "leisure_value": float(leisure),
            "exclude_seniors": is_exclude_seniors,
            "hours_options": hours_list,
            "apply_household_income_filter": {"min": float(inc_min), "max": float(inc_max)},
            "apply_earner_type_filter": earner_type, 
            "apply_household_size_filter": selected_hh_filter
        }

        # Run Backend Logic
        result = load_and_process_model(input_params, force_rerun=force_rerun_val)

        if result['success']:
            # Append new result to history
            new_history = current_history + [result]
            return new_history, f"Run success. Total runs: {len(new_history)}"
        else:
            return current_history, f"Error: {result.get('error')}"

    except Exception as e:
        return current_history, f"Input Error: {str(e)}"


@app.callback(
    [Output('graph-total-hours', 'figure'),
     Output('header-scores', 'children')],
    [Input('history-store', 'data')]
)
def update_graphs(history_data):
    # Initialize Figures
    fig_rate = go.Figure()
    fig_hours = go.Figure()
    header_text = ""

    # Colors for differentiation (optional, Plotly handles this automatically usually)
    try:
        latest_run = history_data[-1]
        header_text = f"Latest Run: {latest_run['score_str']}"
    except IndexError:
        pass

    # Iterate through history to stack plots
    for i, run_data in enumerate(history_data):
        df = pd.DataFrame(run_data['sensitivity'])
        params = run_data['params']
        label_prefix = f"Run {i+1}"

        # --- Graph: Total Employment Hours ---
        fig_hours.add_trace(go.Scatter(
            x=df['scaler'], 
            y=df['total_employment_hrs'],
            mode='lines',
            name=f"{label_prefix} - Total Hrs"
        ))

    # Formatting Graph
    fig_hours.update_layout(
        title="Total Employment Hours Sensitivity",
        xaxis_title="Income Scaler",
        yaxis_title="Employment hours (%)",
        yaxis=dict(tickformat=".0%"), # Formats as percentage
        hovermode="x unified",
        template="plotly_white",
        legend=dict(orientation="h", y=-0.2)
    )
    fig_hours.add_vline(x=1.0, line_dash="dash", line_color="red", opacity=0.5)
    fig_hours.add_hline(y=1.0, line_dash="dash", line_color="blue", opacity=0.5)

    return fig_hours, header_text

@app.callback(
    Output('score-table-container', 'children'),
    Input('history-store', 'data')
)
def update_score_table(history_data):
    if not history_data:
        return dbc.Alert("No runs yet.", color="light")

    # Define Table Header
    table_header = [
        html.Thead(html.Tr([
            html.Th("Run #"),
            html.Th("Total Hrs ($)"),
            html.Th("Min wage ($)"),
            html.Th("Leisure ($)"),
            html.Th("Exclude seniors"),
            html.Th("Hours options"),
            html.Th("Household filter"),
            html.Th("Earner"),
            html.Th("Utility Acc (%)"),
            html.Th("Hours Acc (%)")
        ]))
    ]

    # Define Table Rows
    rows = []
    for i, run in enumerate(history_data):
        params = run['params']
        metrics = run.get('metrics', {'utility_acc': 0, 'hours_acc': 0})
        
        # Color code: Highlight the latest run
        row_style = {"fontWeight": "bold", "backgroundColor": "#f8f9fa"} if i == len(history_data) - 1 else {}

        row = html.Tr([
            html.Td(f"{i + 1}"),
            html.Td(f"{params['total_hours']}"),
            html.Td(f"{params['min_hourly_wage']}"),
            html.Td(f"{params['leisure_value']}"),
            html.Td(f"{params['exclude_seniors']}"),
            html.Td(f"{params['hours_options']}"),
            html.Td(f"{params['apply_household_size_filter']}"),
            html.Td(f"{params['apply_earner_type_filter']}"),
            html.Td(f"{metrics['utility_acc']:.2f}%"),
            html.Td(f"{metrics['hours_acc']:.2f}%"),
        ], style=row_style)
        rows.append(row)

    # Return the full table
    return dbc.Table(
        table_header + [html.Tbody(rows)],
        bordered=True,
        hover=True,
        responsive=True,
        striped=True
    )
if __name__ == "__main__":
    # Create necessary directory if not exists
    os.makedirs("etc/app", exist_ok=True)
    app.run(debug=True)