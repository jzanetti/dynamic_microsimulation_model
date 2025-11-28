
from matplotlib import pyplot as plt
import base64
from io import BytesIO
import math
from os.path import exists
from os import makedirs

def plot_inputs(df, exclude_cols=["id"], plots_per_row=2, output_dir: str = ""):
    """
    Generate an HTML string with distributions for each column in the DataFrame (excluding the specified column).
    - Histograms for numeric columns.
    - Pie charts (percentages) for non-numeric columns.
    Plots are arranged with the specified number per row.
    
    :param df: pandas DataFrame
    :param exclude_col: Column to exclude (default: 'id')
    :param plots_per_row: Number of plots per row (default: 2)
    :return: HTML string
    """
    # Get all columns except excluded
    all_cols = list(df.columns.difference(exclude_cols))
    
    if not all_cols:
        return "<html><body><p>No columns found for distributions.</p></body></html>"
    
    # Separate numeric and non-numeric
    numeric_cols = df[all_cols].select_dtypes(include=['number']).columns.tolist()
    non_numeric_cols = df[all_cols].select_dtypes(exclude=['number']).columns.tolist()
    
    # Combine for plotting order: numeric first, then non-numeric? Or just all in order.
    # For simplicity, plot in original order, but group by type.
    plot_cols = []
    plot_types = {}
    for col in all_cols:
        if col in numeric_cols:
            plot_cols.append(col)
            plot_types[col] = 'hist'
        elif col in non_numeric_cols:
            plot_cols.append(col)
            plot_types[col] = 'pie'
    
    num_plots = len(plot_cols)
    if num_plots == 0:
        return "<html><body><p>No suitable columns found for distributions.</p></body></html>"
    
    # Calculate number of rows needed
    num_rows = math.ceil(num_plots / plots_per_row)
    
    # Start HTML
    html = """
    <html>
    <head>
        <style>
            table { width: 100%; border-collapse: collapse; }
            td { text-align: center; padding: 10px; }
            img { max-width: 100%; height: auto; }
        </style>
    </head>
    <body>
        <table>
    """
    
    plot_index = 0
    for row in range(num_rows):
        html += "<tr>"
        for col_idx in range(plots_per_row):
            if plot_index < num_plots:
                column = plot_cols[plot_index]
                plot_type = plot_types[column]
                
                # Create figure
                fig, ax = plt.subplots(figsize=(6, 4))
                
                if plot_type == 'hist':
                    df[column].hist(ax=ax, bins=20)
                    ax.set_title(f'Distribution of {column} (Histogram)')
                    ax.set_xlabel(column)
                    ax.set_ylabel('Frequency')
                elif plot_type == 'pie':
                    values = df[column].value_counts(normalize=True) * 100  # Percentages
                    if len(values) > 10:  # Limit labels if too many categories
                        values.plot.pie(ax=ax, autopct='%1.1f%%', labels=None)
                        ax.legend(labels=values.index, loc='center left', bbox_to_anchor=(1, 0.5))
                    else:
                        values.plot.pie(ax=ax, autopct='%1.1f%%')
                    ax.set_title(f'Distribution of {column} (Pie Chart - %)')
                    ax.set_ylabel('')  # Remove y-label for pie
                
                # Save to base64
                buffer = BytesIO()
                fig.savefig(buffer, format='png', bbox_inches='tight')
                buffer.seek(0)
                image_base64 = base64.b64encode(buffer.read()).decode('utf-8')
                plt.close(fig)
                
                # Add to HTML
                html += f'<td><img src="data:image/png;base64,{image_base64}" alt="{column} distribution"></td>'
                
                plot_index += 1
            else:
                html += "<td></td>"  # Empty cell if odd number of plots
        html += "</tr>"
    
    html += """
        </table>
    </body>
    </html>
    """
    
    if not exists(output_dir):
        makedirs(output_dir)

    with open(f"{output_dir}/input_distributions.html", "w") as f:
        f.write(html)