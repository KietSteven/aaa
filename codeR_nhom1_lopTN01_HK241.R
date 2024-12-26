# Tiền xử lý số liệu
#3.1 ĐỌc dữ liệu
dirty_data <- read.csv("D:/BTL_XSTK/dirty_data.csv")
head(dirty_data,10)
#3.2 Lam sach du lieu
# chọn lọc biến
new_data <- dirty_data[,c("nearest_warehouse","order_price","delivery_charges",
                          "coupon_discount","order_total","season","distance_to_nearest_warehouse",
                          "is_expedited_delivery","is_happy_customer")]
head(new_data,10)

#Kiểm tra dữ liệu khuyết
library(questionr)
freq.na(new_data)
#kiểm tra cấu trúc dữ liệu
str(dirty_data)
# kiểm định biến định tính, định lượng trong tệp new_data
unique(new_data$season)
unique(new_data$nearest_warehouse)

#chuẩn hóa dữ liệu trong các cột
## for seasons
new_data$nearest_warehouse[new_data$nearest_warehouse == "thompson"] = "Thompson"
new_data$nearest_warehouse[new_data$nearest_warehouse == "nickolson"] = "Nickolson"
new_data$nearest_warehouse[new_data$nearest_warehouse == "bakers"] = "Bakers"
## for nearest_house
new_data$season[new_data$season == "winter"] = "Winter"
new_data$season[new_data$season == "spring"] = "Spring"
new_data$season[new_data$season == "summer"] = "Summer"
new_data$season[new_data$season == "autumn"] = "Autumn"
##truy xuất lại các giá trị khác nhau trong cột
unique(new_data$season)
unique(new_data$nearest_warehouse)

#4.1 thống kê mô tả cho các biến định lượng, định tính
#thống kê cho các biến định lượng, định tính
#định lượng
# tạo hàm basic-stats  tính toán các giá trị thống kê cơ bản cho một vector x:
#mean(x)giá trị trung bình, median(x) giá trị trung vị, sd(x) độ lệch chuẩn, min(x),max(x)
#apply(...,2,basic_stats) hàm apply áp dụng basis_stats cho từng cột(margin=2) trong bảng dữ liệu new_dâta
#gán tên hàng trong bảng table_data
basic_stats <- function(x) {
  stats <- c(mean = mean(x), 
             median = median(x), 
             sd = sd(x), 
             Q1 = quantile(x, 0.25), 
             Q2 = quantile(x, 0.50), 
             Q3 = quantile(x, 0.75), 
             min = min(x), 
             max = max(x))
}
table_data <- apply(new_data[, c("order_price", "delivery_charges",
                                 "coupon_discount", "order_total", "distance_to_nearest_warehouse")], 
                    2, basic_stats)

print(table_data)

#thống kê cho định tính:
table (new_data$nearest_warehouse )
table (new_data$season )
table (new_data$is_expedited_delivery )
table (new_data$is_happy_customer )
#vẽ đồ thị histogram
library(ggplot2)
ggplot(new_data, aes(x = order_total)) + 
  geom_histogram(bins=10,fill = "lightblue") +
  labs(title = "Histogram of Order Total", x = "Order total (USD)",y="Frequency")
ggplot(new_data, aes(x = delivery_charges)) + 
  geom_histogram(bins=10,fill = "lightblue") +
  labs(title = "Histogram of Delivery charges", x = "Delivery charges(USD)",y="Frequency")
ggplot(new_data, aes(x = order_price)) + 
  geom_histogram(bins=10,fill = "lightblue") +
  labs(title = "Histogram of Order price", x = "Order price(USD)",y="Frequency")
ggplot(new_data, aes(x = distance_to_nearest_warehouse)) + 
  geom_histogram(bins=10,fill = "lightblue") +
  labs(title = "Histogram of distance to nearest warehouse", x = "Distance to nearest warehouse(km)",y="Frequency")
#Xử lý ngoại lai cho từng biến gàn cho tệp con new_data2
new_data2 <- new_data
# tạo hàm remove_outliers được định nghĩa với 1 tham số đầu vào là x
#giá trị nào thõa thì thay vào NA, na.rm chỉ đinh rằng cá giá trị bị thiếu (NA) trong dữ liệu sẽ được bỏ qua khi tính toán
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)                                    # Tứ phân vị thứ nhất (Q1)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)                                     # Tứ phân vị thứ ba (Q3)
  IQR_val <- Q3 - Q1                                                      # Khoảng tứ phân vị
  lower <- Q1 - 1.5 * IQR_val                                              # Ngưỡng dưới
  upper <- Q3 + 1.5 * IQR_val                                                    # Ngưỡng trên
  x[x < lower | x > upper] <- NA                                             # Thay ngoại lai bằng NA
  x
}
# áp dụng cho cột cụ thể, questionr cho freq.na
library(questionr)
new_data2$order_total <- remove_outliers(new_data2$order_total)
new_data2$order_price <- remove_outliers(new_data2$order_price)
new_data2$distance_to_nearest_warehouse <- remove_outliers(new_data2$distance_to_nearest_warehouse)
freq.na(new_data2)
# cỡ mẫu length(): Hàm này sẽ trả về số lượng phần tử trong vector đã loại bỏ NA.
#Khi áp dụng cho kết quả của na.omit(), 
#nó sẽ đếm số lượng phần tử không phải NA trong new_data2$delivery_charges.

length(na.omit(new_data2$delivery_charges))
length(na.omit(new_data2$order_total))
length(na.omit(new_data2$order_price))
#vẽ lại đồ thị his khi loại bỏ ngoại lai

hist(new_data2$order_total, 
     xlab = "Order total(USD)", 
     main = "Histogram of order total", 
     col = "lightblue")
hist(new_data2$delivery_charges, 
     xlab = "Delivery charges(USD)", 
     main = "Histogram of delivery charges", 
     col = "lightblue")
hist(new_data2$order_price, 
     xlab = "Order price(USD)", 
     main = "Histogram of order price", 
     col = "lightblue")
hist(new_data2$distance_to_nearest_warehouse, 
     xlab = "Distance to nearest warehouse(km)", 
     main = "Histogram of distance to nearest warehouse", 
     col = "lightblue")
#Kiểm đinh 2 mẫu, thực hiện xóa luôn hàng chứa NA
new_data2 <-na.omit(new_data2)
ggplot(new_data,aes(x=is_happy_customer,y=order_total))+geom_boxplot()+
  labs(title="Plot of iis_happy_customer n per neareast warehouse")
ggplot(new_data2,aes(x=is_happy_customer,y=order_total))+geom_boxplot(fill="lightblue")+
  labs(title="Plot of iis_happy_customer n per neareast warehouse")

ggplot(new_data,aes(x=nearest_warehouse,y=order_total))+geom_boxplot()+
  labs(title="Plot of order total per neareast warehouse")
ggplot(new_data2,aes(x=nearest_warehouse,y=order_total))+geom_boxplot()+
  labs(title="Plot of order total per neareast warehouse")

ggplot(new_data,aes(x=season,y=delivery_charges))+geom_boxplot(fill="lightblue")+
  labs(title="Plot of delivery charges per season")

ggplot(new_data2,aes(x=season,y=order_total))+geom_boxplot(fill="lightblue")+
  labs(title="Plot of delivery charges per season")

ggplot(new_data,aes(x=season,y=order_total))+geom_boxplot(fill="lightblue")+
  labs(title="Plot of delivery charges per season")

ggplot(new_data,aes(x=delivery_charges,y=order_total))+geom_point(,color="darkred")+
  labs(title="delivery charge and Order price scatter plot")

ggplot(new_data,aes(x=order_price,y=order_total))+geom_point(,color="darkred")+
  labs(title="Order total and Order price scatter plot")
ggplot(new_data2,aes(x=order_price,y=order_total))+geom_point(,color="darkred")+
  labs(title="Order total and Order price scatter plot")
#Thong ke suy dien

#Mo hing uoc luong trung binh mot mau
#Do thi
new_data3 <- new_data[c("order_total")]
new_data3 <- remove_outliers(new_data3$order_total)
new_data3 <- na.omit(new_data3)
length(new_data3)
par (mfrow=c(1,2))
hist (new_data3,col=cm.colors(10))
qqnorm (new_data3,col="blue")
qqline (new_data3,col="black")
#Shapiro - Wilk
shapiro.test(new_data3)
#Tim khoang tin cay
N <- length(new_data3)
x <- mean(new_data3)
s <- sqrt(var(new_data3))
z <- qnorm(0.975)
e <- z*s/sqrt(N)
one_sample <- data.frame(
  Tong_the = N,
  Trung_binh = x,
  Do_lech_chuan = s,
  Z = z,
  Nguong_ss = e,
  Can_duoi = x - e,
  Can_tren = x + e)
print (one_sample)
#Kiem dinh trung binh hai mau
#Tach is_happy_customer
two_sample <-new_data[c("is_happy_customer","order_total")]
library(dplyr)
two_sample_cleaned <- two_sample %>%
  group_by(is_happy_customer ) %>%
  mutate(order_total = remove_outliers(order_total)) %>% 
  filter(!is.na(order_total))
table(two_sample_cleaned$is_happy_customer )
TRUE_data <-  two_sample_cleaned[ two_sample_cleaned$
                                    is_happy_customer == 'True',]
FALSE_data <-  two_sample_cleaned[ two_sample_cleaned$
                                     is_happy_customer == 'False',]
#Do thi
par(mfrow =c(1,2))
qqnorm(TRUE_data$order_total,col="lightblue")
qqline(TRUE_data$order_total,col="blue")
qqnorm(FALSE_data$order_total,col="lightblue")
qqline(FALSE_data$order_total,col="blue")
#Shapiro - Wilk
shapiro.test(TRUE_data$order_total)
shapiro.test(FALSE_data$order_total)
#Thuc hien kiem dinh
n1 = length(TRUE_data$order_total)
xtb1 = mean(TRUE_data$order_total)
s1 = sd(TRUE_data$order_total)
n2 = length(FALSE_data$order_total)
xtb2 = mean(FALSE_data$order_total)
s2 = sd(FALSE_data$order_total)
result_two <-data.frame (
  zqs = (xtb1 - xtb2 )/sqrt (s1^2/n1+s2^2/n2),
  Can_duoi = -Inf ,
  Can_tren = - qnorm (0.95))
print ( result_two)
#t
#Kiem dinh phuong sai
var.test (TRUE_data$order_total,FALSE_data$order_total,alternative = "less")
#Kiem dinh t
result_t <- data.frame (
  sp_2 = ((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)->sp2,
  tqs = (xtb1-xtb2)/sqrt(sp2/n1+sp2/n2),
  Can_duoi =-Inf,
  Can_tren = -qt(p=0.05,df=n1+n2-2,lower.tail = FALSE))
print (result_t)
#Mo hinh ANOVA mot nhan to
owa <- new_data[c("order_total","nearest_warehouse")]
owa_cleaned <- owa %>%
  group_by(nearest_warehouse) %>%
  mutate(order_total = remove_outliers(order_total)) %>%
  filter(!is.na(order_total))
table(owa_cleaned$nearest_warehouse)
Bakers <- subset(owa_cleaned,nearest_warehouse =="Bakers")
Thompson <- subset( owa_cleaned,nearest_warehouse =="Thompson")
Nickolson <- subset(owa_cleaned,nearest_warehouse =="Nickolson")
#Dieu kien thuc hien ANOVA
#Kiem ta phan phoi chuan
#Do thi
par (mfrow =c(1,3))
qqnorm (Bakers$order_total,main="QQ Plot for Bakers",cex.main =1.2,
        col ="lightblue")
qqline (Bakers$order_total,col="darkblue")
qqnorm (Thompson$order_total,main="QQ Plot for Thompson",cex.main=1.2,
        col ="lightblue")
qqline (Thompson$order_total,col="darkblue")
qqnorm (Nickolson$order_total,main="QQ Plot for Nickolson",cex.main=1.2,
        col ="lightblue")
qqline (Nickolson$order_total,col="darkblue")
#Shapiro - Wilk
shapiro.test(Bakers$order_total)
shapiro.test(Nickolson$order_total)
shapiro.test(Thompson$order_total)
#Kiem ta su dong nhat phuong sai
#install.packages("car")
library (car)
leveneTest (order_total~as.factor(nearest_warehouse),data = owa_cleaned )
#Thuc hien ANOVA
#Bang thong keke
thongke <- data.frame (
  Warehouse = c("Bakers","Nickolson","Thompson"),
  n = c(length(Bakers$order_total),length(Nickolson$order_total),
        length(Thompson$order_total)),
  Trung_binh = c( mean(Bakers$order_total),mean(Nickolson$order_total),
                  mean(Thompson$order_total)))
print (thongke)
#Tien hanh ANOVA
anova <- aov(order_total~nearest_warehouse,data=owa_cleaned )
summary(anova)
#Mo hinh hoi quy tuyen tinh da boi
#Xay dung mo hinh hoi quy tuyen tinh tot nhat
linear <- new_data[c("delivery_charges","season",
                     "distance_to_nearest_warehouse","is_expedited_delivery",
                     "order_price")]
library(dplyr)
linear <- linear %>%
  mutate(order_price = remove_outliers(order_price)) %>%
  filter(!is.na(order_price))
linear <- linear %>%
  mutate(distance_to_nearest_warehouse = remove_outliers(
    distance_to_nearest_warehouse )) %>%
  filter(!is.na(distance_to_nearest_warehouse))
length(linear$delivery_charges)
set.seed(900)
train.rows <- sample(rownames(linear),dim(linear)[1]*0.8)
train_data <- linear[train.rows,]
test.rows <- setdiff(rownames(linear),train.rows)
test_data <- linear[test.rows,]
model_1 <- lm(delivery_charges~distance_to_nearest_warehouse +season + 
                order_price + is_expedited_delivery,data=train_data)
summary(model_1)
model_2 <- lm(delivery_charges~distance_to_nearest_warehouse + season +
                is_expedited_delivery,data = train_data)
summary (model_2)
#Cac gia dinh cua mo hinh hoi quy
par (mfrow=c(2,2))
plot(model_2,col=cm.colors(10))
#Du doan
test_data$predicted_value <- predict(model_2,test_data)
head(test_data[,c("predicted_value","delivery_charges")],10)
colnames(new_data)
