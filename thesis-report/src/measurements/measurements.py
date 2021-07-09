import pandas as pd


time_columns = ["std_item_detection", "extraction", "verification", "total"]

df = pd.concat(
    [
        pd.read_csv(
            f"measurements-{i+1}.csv",
            index_col="name",
        )
        .dropna(axis=1, how="all")
        .fillna(0)
        .rename(columns=lambda x: x.strip())
        for i in range(6)
    ]
)

mean = df.groupby(df.index).mean()
std = df.groupby(df.index).std()[time_columns]

output = pd.DataFrame()
output["LoC"] = mean["loc"].astype(int)
output["VCs"] = mean["vcs"].astype(int)

for key in time_columns:
    name = key.title().replace("_", " ")
    output[name] = mean[key].apply(lambda f: "${:.1f}\pm".format(f / 1000.0)) + std[
        key
    ].apply(lambda f: "{:.1f}$".format(f / 1000.0))

output.index = output.index.str.title().str.replace("_", "")
output.columns.name = "Name"
output.index.name = None

output.to_latex(
    "measurements.tex",
    escape=False,
    column_format="lrrrrrr",
    label="measurements",
    caption=(
        "Time measurements for all passing test examples of the Rust-Stainless test suite. "
        "The presented times are means and standard errors from 5 runs. "
        "The tests were performed on the \\texttt{mutable-cells} branch, "
        "except for the two tests marked with * which are from \\texttt{master}. "
        "All times are in milliseconds."
    ),
)
