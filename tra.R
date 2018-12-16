#베이직알프로그래밍_(이창현, 고근영, 전가예)
setwd('D:/R')
getwd()

install.packages('reaflet')
library(leaflet)

tra <- function(x1){
  pop_1214<-read.csv("사망교통사고_20122014.csv")
  pop_15<-read.csv("사망교통사고_2015.csv")
  pop_16<-read.csv("사망교통사고_2016.csv")
  pop_17<-read.csv("사망교통사고_2017.csv")
  
  # 2012년~2017년 사이의 전국 사망교통사고
  kor <-rbind(pop_1214,pop_15,pop_16,pop_17)
  
  #사고발생지 시도 추출
  z1 <- kor$발생지시도
  z2 <- sort(unique(z1))
  z3 <- data.frame(z2)

  #사고발생지 시군구 추출
  y1 <- kor$발생지시군구
  y2 <- sort(unique(y1))
  y3 <- data.frame(y2)
  
  
  #초기값설정(지역명 변수,zd:시도, yd:시군구)
  zd <- 0 
  yd <- 0
  
  #발생지 시도인지 판별
  for(k in 1:17){
    if(x1==as.character(z3[k,])){zd <- as.character(z3[k,]) }
  }
  
  #발생지 시군구인지 판별
  if(zd == 0){
    for(i in 1:209){
      if(x1==as.character(y3[i,])){yd <- as.character(y3[i,]) }
    }  
  }
  
  #입력한 지역명의 데이터만 추출
  if(zd == 0){
    dt <- subset(kor,kor$발생지시군구==yd)
  }else{
    dt <- subset(kor,kor$발생지시도==zd)
  }
  
  if(x1 == "전국"){dt <- kor}
  
  #주야 구분 데이터
  day <- subset(dt,dt$주야=="주간")
  night <- subset(dt,dt$주야=="야간")
  
  #'leafelt'패키지 사용
  leaflet(data=dt) %>%  #패키지에서 사용될 데이터입력
    addTiles(group = "일반지도") %>% #일반지도
    addProviderTiles("Stamen.Toner", group = "토너지도") %>%
    addCircles(data=day, #주간데이터
               lng = ~경도,  
               lat = ~위도,
               weight = 2, #선 두께 
               radius = ~sqrt(사상자수) * 130, #원 크기 
               popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년,"<br/>",주야, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
               fillColor = "red", # 원안쪽 색상 
               highlightOptions = highlightOptions( # 포인트이동시 강조 
                 weight = 10,
                 color = "brown",
                 fillColor = "green" ),
               label = ~발생지시도, # 포인트를 올려놓을 시 표시할 문구
               group = "주간") %>%   
    addCircles(data=night, #야간데이터
               lng = ~경도,  
               lat = ~위도,
               weight = 2, #선 두께 
               radius = ~sqrt(사상자수) * 130, #원 크기 
               popup =  ~paste0(발생지시군구,"<br/>연도: ",발생년,"<br/>",주야, "<br/>Frequency: ",사상자수,"<br/>",법규위반),
               fillColor = "red", # 원안쪽 색상 
               highlightOptions = highlightOptions( # 포인트이동시 강조 
                 weight = 10,
                 color = "brown",
                 fillColor = "green" ),
               label = ~발생지시도, # 포인트를 올려놓을 시 표시할 문구
               group = "야간") %>%
    addLayersControl(baseGroups = c("일반지도", "토너지도"), #라디오도구 //표시하고자하는 함수를 group을 지정해주면 된다.
                     overlayGroups = c("주간","야간"), #체크도구
                     options = layersControlOptions(collapsed = FALSE)) #옵션
}

tra("서울")
tra("강남구")
tra("대전")
tra("대덕구")
tra("전국")
tra("청주시")
