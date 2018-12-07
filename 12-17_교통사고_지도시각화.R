#install.packages('leaflet')
library(leaflet)

#2012~2017년도 지역별 교통사고사상자
setwd('H:/R/교통사고_PJ')

getwd()


#대전에서 발생한 교통사고데이터만 추출하기
pop_1214<-read.csv("사망교통사고_20122014.csv")
dg_1214<-subset(pop_1214,pop_1214$발생지시도=="대전")

pop_15<-read.csv("사망교통사고_2015.csv")
dg_15<-subset(pop_15,pop_15$발생지시도=="대전")

pop_16<-read.csv("사망교통사고_2016.csv")
dg_16<-subset(pop_16,pop_16$발생지시도=="대전")

pop_17<-read.csv("사망교통사고_2017.csv")
dg_17<-subset(pop_17,pop_17$발생지시도=="대전")


#4개의 데이터 합치기
fi_dg<-rbind(dg_1214,dg_15,dg_16,dg_17)
View(fi_dg)

#행정구역 나누기
goo1 <-subset(fi_dg,fi_dg$발생지시군구=="대덕구")
goo2 <-subset(fi_dg,fi_dg$발생지시군구=="유성구")
goo3 <-subset(fi_dg,fi_dg$발생지시군구=="서구")
goo4 <-subset(fi_dg,fi_dg$발생지시군구=="중구")
goo5 <-subset(fi_dg,fi_dg$발생지시군구=="동구")


round(fi_dg$위도,2)


#한남대 데이터
hnu <- data.frame(위도 = 127.421036, 경도 = 36.354843)
hnu




#사상자빈도 지도검색
sys <- function(x1){
  
  if(x1 == "사상자수") {
    leaflet(data=fi_dg) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, 주야,"<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
    
  }else if(x1 == "사망자수"){
    leaflet(data=fi_dg) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "중상자수"){
    leaflet(data=fi_dg) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(중상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "경상자수"){
    leaflet(data=fi_dg) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(경상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "대덕구"){
    leaflet(data=goo1) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "유성구"){
    leaflet(data=goo2) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "서구"){
    leaflet(data=goo3) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "중구"){
    leaflet(data=goo4) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "동구"){
    leaflet(data=goo5) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "대덕구, 사상자수"){
    leaflet(data=goo1) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "대덕구, 사망자수"){
    leaflet(data=goo1) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "대덕구, 중상자수"){
    leaflet(data=goo1) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(중상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "대덕구, 경상자수"){
    leaflet(data=goo1) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(경상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "유성구, 사상자수"){
    leaflet(data=goo2) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "유성구, 사망자수"){
    leaflet(data=goo2) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "유성구, 중상자수"){
    leaflet(data=goo2) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(중상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "유성구, 경상자수"){
    leaflet(data=goo2) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(경상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "서구, 사상자수"){
    leaflet(data=goo3) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "서구, 사망자수"){
    leaflet(data=goo3) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "서구, 중상자수"){
    leaflet(data=goo3) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(중상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "서구, 경상자수"){
    leaflet(data=goo3) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(경상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "중구, 사상자수"){
    leaflet(data=goo4) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "중구, 사망자수"){
    leaflet(data=goo4) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "중구, 중상자수"){
    leaflet(data=goo4) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(중상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "중구, 경상자수"){
    leaflet(data=goo4) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(경상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "동구, 사상자수"){
    leaflet(data=goo5) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "동구, 사망자수"){
    leaflet(data=goo5) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(사망자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "동구, 중상자수"){
    leaflet(data=goo5) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(중상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "동구, 경상자수"){
    leaflet(data=goo5) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(경상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }else if(x1 == "한남대"){
    leaflet(data=goo5) %>%
      addTiles() %>%
      addCircles(lng = ~경도,  
                 lat = ~위도,
                 weight = 2, #선 두께 
                 radius = ~sqrt(경상자수) * 130, #원 크기 
                 popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
                 fillColor = "red", # 원안쪽 색상 
                 #fillColor = "transparent",
                 highlightOptions = highlightOptions( # 포인트이동시 강조 
                   weight = 10,
                   color = "brown",
                   fillColor = "green"
                 ),
                 label = ~발생지시도  # 포인트 이동시 표시할 문구 
      )
  }
  
}



sys("사상자수")
sys("사망자수")
sys("중상자수")
sys("경상자수")
sys("대덕구")
sys("유성구")
sys("서구")
sys("중구")
sys("동구")

sys("대덕구, 경상자수")
sys("대덕구, 사망자수")
sys("서구, 사망자수")
sys("중구, 사망자수")
sys("동구, 사망자수")


#이거 만들고싶당
sys("한남대")



?leaflet
