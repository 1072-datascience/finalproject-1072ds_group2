# < 台灣未來空氣品質之預測 >

### Groups
* < 陳嘉葳, 105204031 >
* < 黃品硯, 105703058 >
* < 劉容任, 104703035 >

### Goal
目標為預測台灣未來一天的 PM2.5 為多少。

### Demo 
取得模型的成效:
```R
Rscript code/main.R --fold n --train data/features.csv --report results/performance.csv
```
取得模型的預測成果:
```R
Rscript code/predict.R --train data/features.csv --predict results/predict.csv
```
視覺化(Shiny):
https://willwill.shinyapps.io/testonly/?fbclid=IwAR3GkicbdLGRXDPC0Wl-0EZ9QUP8z5OXD93V4Bl5zRSvuTmb4MmysDjJHm0
![text](https://i.imgur.com/1MYDSFH.png)
## Folder organization and its related information

### docs
Powerpoint:
1072_datascience_FP_Group2.pptx

### data

來源:
- 行政院環保署：https://erdb.epa.gov.tw
- 內政部戶政司
- 經濟部工業局
- 交通部氣象局
- 交通部統計查詢網：https://stat.motc.gov.tw/mocdb/stmain.jsp?sys=100
- 社會經濟資料服務平台：https://segis.moi.gov.tw/stat/web/portal/stat_portalhome.aspx

### code

方法:
- Linear Regression
- Logistic Regression
- Decision Tree
- Gradient Boosting Algorithms

### results

RMSE: 8.37 左右

## Reference
None


