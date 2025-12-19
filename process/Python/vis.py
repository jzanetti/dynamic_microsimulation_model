from matplotlib.pyplot import (
    subplots,
    plot,
    legend,
    title,
    savefig,
    close,
    gca,
    grid,
    xlabel,
    ylabel,
    axvline,
    axhline,
    gca
)
import matplotlib.ticker as mtick
from matplotlib.ticker import MaxNLocator
import base64
from io import BytesIO
import math
from os.path import exists
from process.Python.data.filename import create_hash_filename
from os import makedirs
from pandas import DataFrame
from os.path import join
from pandas import read_csv


def plot_intermediate(input_params: dict, data_name: str, output_dir: str = "/tmp"):

    if data_name == "utility_func":

        filename_hash = create_hash_filename(input_params)
        output_path1 = join(output_dir, f"utility_employment_rate_{filename_hash}.png")
        output_path2 = join(output_dir, f"ruf_total_employment_hrs_{filename_hash}.png")

        sensitivity_results = read_csv(f"{output_dir}/sensitivity_tests_{filename_hash}.csv")
        accuacry_results = read_csv(f"{output_dir}/validation_score_{filename_hash}.csv")

        highest_utility_accuracy = accuacry_results[accuacry_results["scores"] == "highest_utility_accuracy"]["value"].values[0]
        total_hrs_accuracy = accuacry_results[accuacry_results["scores"] == "total_hrs_accuracy"]["value"].values[0]
        r2_mcfadden = accuacry_results[accuacry_results["scores"] == "r2_mcfadden"]["value"].values[0]
        accuacry_score_str = f"Total Utility Accuracy: " + \
            f"{round(highest_utility_accuracy, 2)} %, " + \
            "Total Hours Accuracy: " + \
            f"{round(total_hrs_accuracy, 2)} %; " + \
            "McFadden's R2: " + \
            f"{round(r2_mcfadden, 2)}"

        subplots(figsize=(10, 6))
        plot(sensitivity_results["scaler"], sensitivity_results["full_time"], label=f"Full-time (Working hours >= {input_params["hours_options"][-1]}hr)")
        plot(sensitivity_results["scaler"], sensitivity_results["part_time"], label=f"Part-time (Working hours >= {input_params["hours_options"][1]}hr, < {input_params["hours_options"][-1]}hr)")
        axvline(x=1.0, color='r', linestyle='--')
        axhline(y=1.0, color='b', linestyle='--')
        gca().yaxis.set_major_formatter(mtick.PercentFormatter(xmax=1.0))
        legend()
        title(f"Employment behaviours with Random Utility Function \n {accuacry_score_str}")
        xlabel("Income Scaler")
        ylabel("Employment rate")
        savefig(output_path1, bbox_inches="tight")
        close()

        subplots(figsize=(10, 6))
        plot(sensitivity_results["scaler"], sensitivity_results["total_employment_hrs"])
        axvline(x=1.0, color='r', linestyle='--')
        axhline(y=1.0, color='b', linestyle='--')
        gca().yaxis.set_major_formatter(mtick.PercentFormatter(xmax=1.0))
        title(
            f"Employment behaviours with Random Utility Function \n {accuacry_score_str}"
        )
        xlabel("Income Scaler")
        ylabel("Employment hours")
        savefig(output_path2, bbox_inches="tight")
        close()

        print(f"The results are written with hashname: {filename_hash}")


def plot_outputs(output_results: DataFrame, output_dir: str = ""):

    # Plot population status
    subplots(figsize=(10, 6))
    proc_data = output_results["population"]["pop"]
    plot(
        proc_data[proc_data["life_stage"] == "alive"]["year"],
        proc_data[proc_data["life_stage"] == "alive"]["count"],
        label="alive",
    )
    plot(
        proc_data[proc_data["life_stage"] == "dead"]["year"],
        proc_data[proc_data["life_stage"] == "dead"]["count"],
        label="dead",
    )
    grid()
    legend()
    ax = gca()
    ax.xaxis.set_major_locator(MaxNLocator(integer=True))
    title("Number of people: Alive/Dead")
    savefig(join(output_dir, "results_pop_stats.png"), bbox_inches="tight")
    close()

    # Plot household status
    subplots(figsize=(10, 6))
    proc_data = output_results["population"]["hhd"]
    plot(proc_data["year"], proc_data["count"])
    grid()
    ax = gca()
    ax.xaxis.set_major_locator(MaxNLocator(integer=True))
    title("Number of household")
    savefig(join(output_dir, "results_hhd_stats.png"), bbox_inches="tight")
    close()

    # Plot employment status
    subplots(figsize=(10, 6))
    proc_data = output_results["employment"]["income"]
    for proc_key in [
        "mean_market_income_per_week",
        "mean_latent_market_income_per_week",
    ]:
        plot(proc_data[proc_key]["year"], proc_data[proc_key][proc_key], label=proc_key)
    ax = gca()
    grid()
    legend()
    ax.xaxis.set_major_locator(MaxNLocator(integer=True))
    title("Income Statistics (Per Week)")
    savefig(join(output_dir, "employment_income.png"), bbox_inches="tight")
    close()


def plot_inputs(
    df: DataFrame, exclude_cols=["id"], plots_per_row=2, output_dir: str = ""
):
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
    numeric_cols = df[all_cols].select_dtypes(include=["number"]).columns.tolist()
    non_numeric_cols = df[all_cols].select_dtypes(exclude=["number"]).columns.tolist()

    # Combine for plotting order: numeric first, then non-numeric? Or just all in order.
    # For simplicity, plot in original order, but group by type.
    plot_cols = []
    plot_types = {}
    for col in all_cols:
        if col in numeric_cols:
            plot_cols.append(col)
            plot_types[col] = "hist"
        elif col in non_numeric_cols:
            plot_cols.append(col)
            plot_types[col] = "pie"

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
                fig, ax = subplots(figsize=(6, 4))

                if plot_type == "hist":
                    df[column].hist(ax=ax, bins=20)
                    ax.set_title(f"Distribution of {column} (Histogram)")
                    ax.set_xlabel(column)
                    ax.set_ylabel("Frequency")
                elif plot_type == "pie":
                    values = (
                        df[column].value_counts(normalize=True) * 100
                    )  # Percentages
                    if len(values) > 10:  # Limit labels if too many categories
                        values.plot.pie(ax=ax, autopct="%1.1f%%", labels=None)
                        ax.legend(
                            labels=values.index,
                            loc="center left",
                            bbox_to_anchor=(1, 0.5),
                        )
                    else:
                        values.plot.pie(ax=ax, autopct="%1.1f%%")
                    ax.set_title(f"Distribution of {column} (Pie Chart - %)")
                    ax.set_ylabel("")  # Remove y-label for pie

                # Save to base64
                buffer = BytesIO()
                fig.savefig(buffer, format="png", bbox_inches="tight")
                buffer.seek(0)
                image_base64 = base64.b64encode(buffer.read()).decode("utf-8")
                close(fig)

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
