import pandas as pd

sales = pd.read_csv(r"C:\Users\Konstantin_2\Downloads\supermarket_sales - Sheet1.csv")

sales["Date"] = pd.to_datetime(sales["Date"])

sales.to_csv(r"C:\Users\Konstantin_2\Downloads\supermarket_neu2.csv", sep = ",", index=False)

print(sales.info())


