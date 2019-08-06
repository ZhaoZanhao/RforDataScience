
# R数据科学

# 安装包
# install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
library(maps)



# 本书的第一部分内容：探索

# 第一章：使用ggplot2进行数据可视化分析
mpg
# View(mpg)

# displ 引擎大小
# hwy 汽车在高速公路上行驶时的燃油效率
# 创建ggplot图形
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy))
# 显示每辆车的类型
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy, color=class))
# 故意显示警告
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy, size=class))
# 透明度
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy, alpha=class))
# shape
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy, shape=class))
# 手动为几何对象设置图形属性
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy), color="blue")
# 分面
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy))+facet_wrap(~class,nrow=2)
# 两个变量进行分面
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy))+facet_grid(drv~cyl)
ggplot(data=mpg)+geom_point(mapping=aes(x=displ, y=hwy))+facet_grid(.~cyl)

# 几何对象
ggplot(data=mpg)+geom_smooth(mapping=aes(x=displ, y=hwy, linetype=drv))

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, color = drv),show.legend = FALSE)

# 在一张图中显示多个几何对象
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data=mpg, mapping=aes(x=displ, y=hwy))+geom_point()+geom_smooth()

# 在不同图层中显示不同的图形属性
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(color=class))+
  geom_smooth()

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(mapping = aes(color=class))+
  geom_smooth(data=filter(mpg, class=="subcompact"), se=FALSE)

# 统计变换
ggplot(data=diamonds)+
  geom_bar(mapping = aes(x=cut))


ggplot(data=diamonds)+
  stat_count(mapping = aes(x=cut))

demo <- tribble(~a,    ~b,
                "bar1",20,
                "bar2",30,
                "bar3",40)
ggplot(data=demo)+
  geom_bar(
    mapping = aes(x=a,y=b),stat="identity"
  )

ggplot(data=diamonds)+
  geom_bar(
    mapping = aes(x=cut,y=..prop.., group=1)
  )

ggplot(data=diamonds)+
  stat_summary(
    mapping = aes(x=cut, y=depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# 位置调整
# 条形图可以用color或者fill图形属性来为图形上色
ggplot(data=diamonds)+
  geom_bar(mapping = aes(x=cut, color=cut))

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill=cut))

# 将fill映射到另外一个变量
ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill=clarity))

ggplot(data = diamonds, mapping = aes(x=cut, fill=clarity))+
  geom_bar(alpha=1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x=cut, color=clarity))+
  geom_bar(fill=NA, position = "identity")

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill=clarity), position = "fill")

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut, fill=clarity), position = "dodge")

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy), position = "jitter")

# 坐标系
ggplot(data=mpg, mapping = aes(x=class, y=hwy)) + 
  geom_boxplot()

ggplot(data=mpg, mapping = aes(x=class, y=hwy)) + 
  geom_boxplot() + 
  coord_flip()

# 为地图设置合适的纵横比
nz <- map_data("nz")
ggplot(nz, aes(long, lat, group=group))+
  geom_polygon(fill="white", color="black")

ggplot(nz, aes(long, lat, group=group))+
  geom_polygon(fill="white", color="black")+
  coord_quickmap()

# 极坐标
bar <- ggplot(data=diamonds)+
  geom_bar(
    mapping = aes(x=cut, fill=cut),
    show.legend = FALSE,
    width = 1
  )+
  theme(aspect.ratio = 1) + labs(x=NULL, y=NULL)

bar+coord_flip()
bar+coord_polar()

# 第3章

library(dplyr)
library(tidyverse)
library(zeallot)
library(nycflights13)

nycflights13::flights

# 按照条件筛选行
filter(flights, month==1, day==1)
jan1 <- filter(flights, month==1, day==1)
day25 <- filter(flights, month==12, day==25)
(day25 <- filter(flights, month==12, day==25))

# 浮点数
near(sqrt(2)^2, 2)

filter(flights, month==11 | month==12)

# 简写形式
# x %in% y 
# 选取出x是y中的一个值对应的行
nov_dec <- filter(flights, month %in% c(11,12))
nov_dec

# 将筛选条件进行简化
# !(x&y) 等于 !x | !y
# !(x|y) 等于 !x & !y

filter(flights, !(arr_delay>120 | dep_delay>120))
filter(flights, arr_delay<=120 | dep_delay<=120)


df <- tibble(x=c(1,NA,3))
filter(df, x>1)
filter(df,is.na(x) | x>1)

# 对选择变量的观测进行排序
arrange(flights,year,month,day)
arrange(flights, desc(arr_delay))

df <- tibble(x=c(5,2,NA))
arrange(df, x)
arrange(df,desc(x))

# 选择变量
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, starts_with("y"))
select(flights, ends_with("y"))
select(flights, contains("y"))
select(flights, num_range("x",1:3))
rename(flights, tail_num=tailnum)
select(flights, time_hour, air_time, everything())

# 增加新变量
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain=arr_delay-dep_delay, speed=distance/air_time*60)
mutate(flights_sml, gain=arr_delay-dep_delay, hours=air_time/60, gain_per_hour=gain/hours)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

# 创建常用函数
# 模运算符 %/% 整数除法 和 %% 求余数
transmute(flights, dep_time, hour=dep_time %/% 100, minute=dep_time %% 100)
# 对数函数log()、log2()、log10()
# 偏移函数lead()领先值 lag()滞后值
(x <- 1:10)
lag(x)
lead(x)
# 领先值和滞后值为相反
x
cumsum(x) # 累积和
cummean(x) # 累积平均值
y <- c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

# 进行分组摘要
summarize(flights, delay=mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights,year,month,day)
summarize(by_day, delay=mean(dep_delay, na.rm = TRUE))

# 使用管道进行操作
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, count=n(), dist=mean(distance, na.rm = TRUE), delay=mean(arr_delay, na.rm = TRUE))
delay
delay <- filter(delay, count>20, dest !="HNL")
delay

ggplot(data = delay, mapping = aes(x=dist, y=delay))+
  geom_point(aes(size=count), alpha=1/3)+
  geom_smooth(se=FALSE)

delays <- flights %>% 
  group_by(dest) %>% 
  summarize(count=n(), dist=mean(distance, na.rm = TRUE), delay=mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count>20, dest != "HML")

# 缺失值
flights %>% 
  group_by(year, month, day) %>% 
  summarize(mean=mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarize(mean=mean(dep_delay, na.rm=TRUE))

# 除去取消的航班
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean=mean(dep_delay))

# 计算
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(delay=mean(arr_delay)) %>% 
  
ggplot(data=delays, mapping=aes(x=delay))+
  geom_freqpoly(binwidth=10)

# count(n)
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(delay=mean(arr_delay, na.rm = TRUE), n=n())

ggplot(delays, mapping = aes(x=n, y=delay))+
  geom_point(alpha=1/10)

delays %>% 
  filter(n>25) %>% 
  ggplot(mapping = aes(x=n, y=delay))+
  geom_point(alpha=1/10)

# 安装包
library(Lahman)
Batting
# 转化成tibble, 以便输出更加美观
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarize( ba=sum(H, na.rm = TRUE)/sum(AB, na.rm = TRUE),
             ab=sum(AB, na.rm = TRUE))

batters %>% 
  filter(ab>100) %>% 
  ggplot(mapping = aes(x=ab, y=ba))+
  geom_point()+
  geom_smooth(se=FALSE)

batters %>% 
  arrange(desc(ba))

# 常用的摘要函数
# mean() 位置函数
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    # 平均延误时间
    avg_delay1=mean(arr_delay),
    # 平均正延误时间
    avg_delay2=mean(arr_delay[arr_delay>0])
  )

# 分散程度度量
# sd()标准差 
# IQR()四分位距
# mad()绝对中位差

not_cancelled %>% 
  group_by(dest) %>% 
  summarize(distance_sd=sd(distance)) %>% 
  arrange(desc(distance_sd))

# 秩的度量
# max() min() quantile(x,0.25)

# 每天最早和最晚是什么时候出发的
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    first=min(dep_time),
    last=max(dep_time)
  )

# 定位度量
# first() 
# nth(x,2)
# last()
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    fisrt_dep=first(dep_time),
    last_dep =last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r=min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

# 计数
# 非缺失值的数量，可以选用sum(!is.na(x))
# 想要计算出唯一值的数量，可以选用n_distinct(x)

# 计算哪个目的地有最多的航空公司
not_cancelled %>% 
  group_by(dest) %>% 
  summarize(carriers=n_distinct(carrier)) %>% 
  arrange(desc(carriers))

# 因为计数很常见
# 因此dplyr提供了一个函数
not_cancelled %>% 
  count(dest)
not_cancelled %>% 
  count(tailnum, wt=distance)

# 多少架航班是在早上5点前出发的
not_cancelled %>% 
  group_by(year,month,day) %>% 
  summarize(n_early=sum(dep_time<500))

# 延误超过1个小时的航班比率
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(hour_pec=mean(arr_delay>60))

# 按照多个变量进行分组
daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights=n()))
(per_month <- summarize(per_day, flights=sum(flights)))  
(per_year <- summarize(per_month, flights=sum(flights)))

# 如果想取消分组，那么可以使用ungroup()函数
daily %>% 
  ungroup() %>% 
  summarize(flights=n())

# 分组新变量和筛选器
flights_sml %>% 
  group_by(year,month,day) %>% 
  filter(rank(desc(arr_delay))<18)

# 找出大于某个阈值的所有分组
popular_dest <- flights %>% 
  group_by(dest) %>% 
  filter(n()>365)
popular_dest

popular_dest %>% 
  filter(arr_delay>0) %>% 
  mutate(prop_delay=arr_delay/sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# 第四章 脚本

# 第五章 探索性数据分析

# 对分布进行可视化表示
ggplot(data=diamonds)+
  geom_bar(mapping = aes(x=cut))
diamonds %>%
  count(cut)

ggplot(data=diamonds)+
  geom_histogram(mapping=aes(x=carat), binwidth = 0.5)

diamonds %>%
  count(cut_width(carat, 0.5))

smaller <- diamonds %>%
  filter(carat < 3)
# ggplot(as.data.frame(smaller), mapping = aes(x=carat))
# +geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

# 典型值
ggplot(data=smaller, mapping=aes(x=carat))+
  geom_histogram(binwidth = 0.01)

ggplot(data=faithful, mapping = aes(x=eruptions))+
  geom_histogram(binwidth = 0.25)

ggplot(diamonds)+
  geom_histogram(mapping = aes(x=y), binwidth = 0.5)

ggplot(diamonds)+
  geom_histogram(mapping = aes(x=y), binwidth = 0.5)+
  coord_cartesian(ylim = c(0,50))

unusual <- diamonds %>% 
  filter(y<3 | y>20) %>% 
  arrange(y)
unusual

# 缺失值
diamonds2 <- diamonds %>% 
  filter(between(y,3,20))
diamonds2 <- diamonds %>%
  mutate(y=ifelse(y<3 | y>30,NA,y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

flights %>%
  mutate(
    cancelled=is.na(dep_time),
    sched_hour=sched_dep_time %/% 100,
    sched_min=sched_dep_time %% 100,
    sched_dep_time=sched_hour+sched_min/60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time))+
  geom_freqpoly(
    mapping = aes(color=cancelled),
    binwidth=1/4
  )

# 相关变量
ggplot(data=diamonds, mapping = aes(x=price))+
  geom_freqpoly(mapping = aes(color=cut),binwidth=500)

ggplot(data=diamonds)+geom_bar(mapping = aes(x=cut))

ggplot(diamonds, mapping = aes(x=price, y=..density..))+
  geom_freqpoly(mapping = aes(color=cut), binwidth=500)

# 箱线图
ggplot(data=diamonds, mapping = aes(x=cut, y=price))+
  geom_boxplot()

# 质量更好的钻石的平均价格更低

# 重新排序
ggplot(data=mpg, mapping = aes(x=class, y=hwy))+
  geom_boxplot()

ggplot(data=mpg, mapping = aes(x=reorder(class, hwy, FUN=median), y=hwy))+
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )

# 旋转90度
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )+
  coord_flip()

# 两个分类变量
ggplot(data=diamonds)+
  geom_count(mapping = aes(x=cut, y=color))

diamonds %>%
  count(color, cut)

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x=color, y=cut))+
  geom_tile(mapping = aes(fill=n))

# library(seriation)
# library(d3headtmap)
# libarary(heatmaply)

# 两个连续变量
ggplot(data = diamonds)+
  geom_point(mapping = aes(x=carat, y=price))

ggplot(data = diamonds)+
  geom_point(mapping = aes(x=carat, y=price), alpha=1/100)

# 透明度+分箱
# 一个维度分箱：geom_histogram()和geom_freqploy()
# 两个维度分箱：geom_bin2d()和geom_hex()
# geom_bin2d()创建长方形分箱
# geom_hex()创建六边形分箱 依赖包hexbin package
# install.packages("hexbin")
library(hexbin)

ggplot(data=smaller)+
  geom_bin2d(mapping = aes(x=carat, y=price))

# install.packages("nexbin")
ggplot(data = smaller)+
  geom_hex(mapping = aes(x=carat, y=price))

ggplot(data = smaller, mapping = aes(x=carat, y=price))+
  geom_boxplot(mapping = aes(group=cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x=carat, y=price))+
  geom_boxplot(mapping = aes(group=cut_number(carat, 20)))

# 模式和模型
ggplot(data = faithful)+
  geom_point(mapping = aes(x=eruptions, y=waiting))

# install.packages("modelr")
library(modelr)
mod <- lm(log(price)~log(carat), data=diamonds)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>%
  mutate(resid=exp(resid))

ggplot(data=diamonds2)+
  geom_point(mapping = aes(x=carat, y=resid))

ggplot(data = diamonds2)+
  geom_boxplot(mapping = aes(x=cut, y=resid))

# ggplot2调用
ggplot(data=faithful, mapping = aes(x=eruptions))+
  geom_freqpoly(binwidth=0.25)

# 精简写法
ggplot(faithful, aes(eruptions))+
  geom_freqpoly(binwidth=0.25)

diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill=n))+
  geom_tile()

# 第六章：工作流 项目

# tibble实现简单数据框
as_tibble(iris)

tibble(
  x = 1:5,
  y = 1,
  z = x^2+y
)

tb <- tibble(
  ':)'   = "smile",
  ' '    = "space",
  '2000' = "number"
)
tb

# 函数tribble()
tribble(
  ~x, ~y, ~z,
############
  "a", 2, 3.6,
  "b", 1, 8.5
)

# 对比tibble和data.frame
tibble(
  a=lubridate::now()+runif(1e3)*86400,
  b=lubridate::today()+runif(1e3)*30,
  c=1:1e3,
  d=runif(1e3),
  e=sample(letters, 1e3, replace = TRUE)
)

# 打印设置
nycflights13::flights %>%
  print(n=10, width=Inf)
# n为观测，width=Inf可以打印所有变量

# 可以设置以下选项来控制默认的输出方式
# options(tibble.print_max = n, tibble.print_min = m) 如果多于m行，则只打印n行
# options(tibble.print_min = Inf) # 打印所有行
# options(tibble.width = Inf) # 表示答应所有的列，不考虑屏幕的宽度

# package?tibble
# 内置查看器
nycflights13::flights %>%
  View()

# 取子集
df <- tibble(
  x=runif(5),
  y=rnorm(5)
)

# 按照名称提取
df$x
df$y
df[["x"]]

# 想要在管道中使用这些提取操作，需要使用特殊的占位符
df %>% .$x
df %>% .[["x"]]

# 与旧代码进行交互
class(as.data.frame(df))

# 使用readr进行数据导入
# read_csv()读取逗号分隔符文件
# read_csv2()读取分号分隔符文件
# read_tsv()读取制表分隔符文件
# read_delim()可以读取任意分隔符文件
# read_fwf()读取固定宽度的文件
# read_table()
# heights <- read_csv("data/heights.csv")
read_csv("a,b,c
         1,2,3
         4,5,6")

read_csv("The first line of metadata
The second line of metadata
x,y,z
1,2,3", skip = 2)

read_csv("# A comment I want to skip
x,y,z
1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
read_csv("a,b,c\n1,2,.", na = ".")

library(tcltk)
u <- 1:2000 
#开启进度条 
pb <- tkProgressBar("进度","已完成 %", 0, 100) 
for(i in u) {   
  info<- sprintf("已完成 %d%%", round(i*100/length(u)))   
  setTkProgressBar(pb, i*100/length(u), sprintf("进度 (%s)", info),info)
  }   
#关闭进度条
close(pb)

# 解析向量
str(parse_logical(c("TRUE","FALSE","NA")))
str(parse_integer(c("1","2","3")))
str(parse_date(c("2010-01-01","1979-10-14")))

parse_integer(c("123","345",".","456"), na=".")
x <- parse_integer(c("123","345","abc","123.45"))
x
problems(x)


# parse_logical() 和parse_integer() 函数分别解析逻辑值和整数。因为这两个解析函数
# 基本不会出现问题，所以我们不再进行更多介绍。
# parse_double() 是严格的数值型解析函数，parse_number() 则是灵活的数值型解析函数。
# 这两个函数要比你预想的更复杂，因为世界各地书写数值的方式不尽相同。
# parse_character() 函数似乎太过简单，甚至没必要存在。但一个棘手的问题使得这个
# 函数变得非常重要：字符编码。
# parse_factor() 函数可以创建因子，R 使用这种数据结构来表示分类变量，该变量具有
# 固定数目的已知值。
# parse_datetime()、parse_date() 和parse_time() 函数可以解析不同类型的日期和时间。
# 它们是最复杂的，因为有太多不同的日期书写形式。

# 数值
parse_double("1.23")
parse_double("1,23")
parse_double("1,23", locale = locale(decimal_mark = ","))

# parse_number() 忽略数值后的非数值字符
parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")

# 适用美国地区
parse_number("$123,456,789")
# 适用于多数欧洲地区
parse_number("123.456.789", locale = locale(grouping_mark = "."))
# 适用于瑞士
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

# 字符串
charToRaw("Zhaozanhao")

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
# 编码方式
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw("x1"))
guess_encoding(charToRaw("x2"))

# 因子
fruit <- c("apple","banana")
parse_factor(c("apple","banana","bananana"), levels = fruit)

# 日期、日期时间与时间
# 根据需要的是日期型数据（从1970-01-01 开始的天数）
# 日期时间型数据（从1970-01-01午夜开始的秒数）
# 或者是时间型数据（从午夜开始的秒数）

# parse_datetime() 期待的是符合ISO 8601 标准的日期时间
parse_datetime("2010-10-01T2010")
# 时间被忽略
parse_datetime("20101010")

# parse_date() 期待的是四位数的年份-月-日 年/月/日 
parse_date("2012-12-16")

# parse_time() 期待的是小时、:、分钟、可选的: 和秒，以及一个可选的a.m./p.m. 标识符
library(hms)
parse_time("01:01 am")
parse_time("01:01:01")

# 因为R 基础包中没有能够很好表示时间数据的内置类，所以我们使用hms 包提供的时间类。
# 如果这些默认设置不适合你的数据，那么你可以提供自己的日期时间格式，格式由以下各
# 部分组成。
# 年
# %Y（4 位数）。
# %y（2 位数；00-69 → 2000-2069、70-99 → 1970-1999）。
# 月
# %m（2 位数）。
# %b（简写名称，如Jan）。
# %B（完整名称，如January）。
# 日
# %d（1 位或2 位数）。
# %e（2 位数）
# 时间
# %H（0-23 小时）。
# %I（0-12 小时，必须和%p 一起使用）。
# %p（表示a.m./p.m.）。
# %M（分钟）。
# %S（整数秒）。
# %OS（实数秒）。
# %Z（时区，America/Chicage 这样的名称）。注意，要当心缩写。如果你是美国人，注意
# EST 是加拿大没有夏时制的一个时区。它表示东部标准时间！我们还会在12.5 节中继
# 续讨论这个话题。
# %z（与国际标准时间的时差，如+0800）。
# 非数值字符
# %.（跳过一个非数值字符）。
# %*（跳过所有非数值字符）。
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")

# 如果对非英语月份名称使用%b 或%B，那么你就需要在locale() 函数中设置lang 参数。查
# 看date_names_langs() 函数中的内置语言列表，如果你的语言没有包括在内，那么可以使
# 用date_names() 函数创建自己的月份和日期名称：
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

# 解析文件
# readr 使用一种启发式过程来确定每列的类型：先读取文件的前1000 行，然后使用（相对保
# 守的）某种启发式算法确定每列的类型。可以使用字符向量模拟这个过程，先使用guess_
# parser() 函数返回readr 最可信的猜测，接着parse_guess() 函数使用这个猜测来解析列：
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TURE","FALSE"))
guess_parser(c("1","5","9"))
guess_parser("12,358,555")
str(guess_parser("2019-08-03"))

challenge <- read_csv(readr_example("challenge.csv"))

# readr_example 可以找到包含在R中的文件的路径
problems(challenge)
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_integer(),
    y = col_character()
  )
)
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)

tail(challenge)

# 设定y为日期列
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_type = cols(
  x = col_double(),
  y = col_date()
)
)

tail(challenge)

# 其他测量
challenge2 <- read_csv(readr_example("challenge.csv"),
                       guess_max = 1001)
challenge2

challenge2 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character())
)

df <- tribble(
  ~x, ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)

df

type_convert(df)

# 写入文件
write_csv(challenge, "challenge.csv")
challenge
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")

# write_rds() 和read_rds() 函数是对基础函数readRDS() 和saveRDS() 的统一包装。前
# 者可以将数据保存为R 自定义的二进制格式，称为RDS 格式：
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

# install.packages("feather")
library(feather)

# 其他类型的数据

# 使用dplyr处理关系数据

# 三类操作来处理关系数据
# 合并连接
# 筛选连接
# 集合操作

# 关系数据库管理系统
# RDBMS
library(tidyverse)
library(nycflights13)
# airlines：可以根据航空公司的缩写码查到公司全名。
airlines
# airports：给出了每个机场的信息，通过faa 机场编码进行标识。
airports
# planes：给出了每架飞机的信息，通过tailnum 进行标识。
planes
# weather：给出了纽约机场每小时的天气状况。
weather

# ??? flights 与planes 通过单变量tailnum 相连；
# ??? flights 与airlines 通过变量carrier 相连；
# ??? flights 与airports 通过两种方式相连（变量origin 和dest）；
# ??? flights 与weather 通过变量origin（位置）以及year、month、day 和hour（时间）相连
planes %>% 
  count(tailnum) %>% 
  filter(n>1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n>1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n>1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n>1)

# 合并连接
flights2 <- flights %>% 
  count(year, month, day, hour, origin, dest, tailnum, carrier) %>% 
  View()


flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")

flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

# 理解连接
x <- tribble(
  ~key, ~val_x,
  #---/ ------
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# 内连接
x %>% 
  inner_join(y, by="key")

# 外连接 ： 左连接 右连接 全连接

# 全连接 ：full_join(x,y)

# 重复键
# 一张表中有重复键
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)

left_join(x, y, by = "key")

# 两张表有重复键
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

# 定义键列
# 迄今为止，两张表都是通过一个单变量来连接的，而且这个变量在两张表中具有同样的名
# 称。这种限制条件是通过by = "key" 来实现的。你还可以对by 设置其他值，以另外的方
# 式来连接表。

flights2 %>%
  left_join(weather)

flights2 %>% 
  left_join(planes, by="tailnum")

flights2 %>% 
  left_join(airports, c("dest"="faa"))

flights2 %>% 
  left_join(airports, c("origin"="faa"))

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# inner_join(x, y) merge(x, y)
# left_join(x, y)  merge(x, y, all.x = TRUE)
# right_join(x, y) merge(x, y, all.y = TRUE)
# full_join(x, y)  merge(x, y, all.x = TRUE, all.y = TRUE)

# dplyr 连接操作的优点是，可以更加清晰地表达出代码的意图：不同连接间的区别确实非常
# 重要，但隐藏在merge() 函数的参数中了。dplyr 连接操作的速度明显更快，而且不会弄乱
# 行的顺序。


# inner_join(x, y, by = "z") SELECT * FROM x INNER JOIN y USING (z)
# left_join(x, y, by = "z")  SELECT * FROM x LEFT OUTER JOIN y USING (z)
# right_join(x, y, by = "z") SELECT * FROM x RIGHT OUTER JOIN y USING (z)
# full_join(x, y, by = "z")  SELECT * FROM x FULL OUTER JOIN y USING (z)

# 合并连接：影响变量
# 筛选连接：影响观测
# semi_join(x, y)：保留x 表中与y 表中的观测相匹配的所有观测。
# anti_join(x, y)：丢弃x 表中与y 表中的观测相匹配的所有观测。

# 最欢迎的目的地
top_dest <- flights %>% count(dest, sort = TRUE) %>% head(10)
top_dest  

# 找出这些航班
flights %>% 
  filter(dest %in% top_dest$dest) %>% 
  View()

# 此时你应该使用半连接，它可以像合并连接一样连接两个表，但不添加新列，而是保留x
# 表中那些可以匹配y 表的行：
flights %>% 
  semi_join(top_dest)

# 半连接的逆操作是反连接。反连接保留x 表中那些没有匹配y 表的行。
flights %>% 
  anti_join(planes, by="tailnum") %>% 
  count(tailnum, sort = TRUE)

# 连接中的问题
# 首先，需要找出每个表中可以作为主键的变量。
# 确保主键中的每个变量都没有缺失值。
# 检查外键是否与另一张表的主键相匹配。
airports %>% count(alt, lon) %>% filter(n > 1)

# 集合操作
# 两表之间的最后一种操作就是集合操作。我们通常很少使用这种操作，但如果你想要将一个复
# 杂的筛选操作分解为多个简单部分时，它们还是有些用处的。所有集合操作都是作用于整行
# 的，比较的是每个变量的值。集合操作需要x 和y 具有相同的变量，并将观测按照集合来处理。

# intersect(x, y)
# 返回既在x 表，又在y 表中的观测。
# union(x, y)
# 返回x 表或y 表中的唯一观测。
# setdiff(x, y)
# 返回在x 表，但不在y 表中的观测。

df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)

df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)

intersect(df1,df2)
union(df1,df2)
setdiff(df1,df2)
setdiff(df2,df1)

# 第10章
# 使用stringr处理字符串
# 本章将介绍R 中的字符串处理。你将学习字符串的基本工作原理，以及如何手工创建字符
# 串，但本章的重点是正则表达式（regular expression，regexp）。正则表达式的用处非常大，
# 字符串通常包含的是非结构化或半结构化数据，正则表达式可以用简练的语言来描述字符
# 串中的模式。第一次见到正则表达式时，你可能会认为它是猫在键盘上踩出来的，但随着
# 逐渐加深对它的理解后，你就能体会其中的深刻含义了。

library(tidyverse)
library(stringr)
# 字符串基础
string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'
string1
string2

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
double_quote
single_quote

# 字符串的打印形式与其本身的内容不是相同的，因为打印形式中会显示出转义字
# 符。如果想要查看字符串的初始内容，可以使用writeLines() 函数：
x <- c("\"", "\\")
x
writeLines(x)

# 还有其他几种特殊字符。最常用的是换行符\n 和制表符\t，你可以使用?'"' 或?"'" 调
# 出帮助文件来查看完整的特殊字符列表。有时你还会看到"\u00b5" 这样的字符串，这是一
# 种在所有平台上都有效的非英文字符的写法：
x <- "\u00b5"
x
writeLines(x)
c("one", "two", "three")

# R 基础包中包含了很多字符串处理函数，但我们尽量不使用这些函数，因为它们的使用方
# 法不一致，很难记忆。相反，我们将使用stringr 中的函数，这些函数的名称更直观，并且
# 都是以str_ 开头的。
str_length(c("a", "R for Data Science", NA))

# 字符串组合
str_c("x","y")
str_c("x","y","z")
str_c("x","y",sep=",")

x <- c("abc",NA)
str_c("|-", x, "-|")
# 防止缺失值传染
str_c("|-", str_replace_na(x), "-|")

# str_c() 函数是向量化的，它可以自动循环短向量，使得其与最长的向
# 量具有相同的长度：
str_c("prefix-", c("a", "b", "c"), "-suffix")

# 长度为0 的对象会被无声无息地丢弃。这与if 结合起来特别有用：
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE
str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

# 要想将字符向量合并为字符串，可以使用collapse() 函数：
str_c(c("x","y","z"), collapse = ",")

# 字符串求子集
# 可以使用str_sub() 函数来提取字符串的一部分。除了字符串参数外，str_sub() 函数中还
# 有start 和end 参数，它们给出了子串的位置（包括start 和end 在内）：
x <- c("Apple","Banana","Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a",1,3)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

# 区域设置
# str_to_lower() 函数将文本转换为小写，你还可以使用str_to_upper() 或 str_to_title() 函数
str_to_upper(c("i", "??"))
str_to_upper(c("i", "??"), locale = "tr")

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x, locale = "haw")

# 使用正则表达式进行模型匹配
# 正则表达式是一门非常精练的语言，可以描述字符串中的模式。理解正则表达式需要花费
# 一点精力，但是一旦理解了，你就会发现其功能如此强大。
# 我们通过str_view() 和str_view_all() 函数来学习正则表达式。这两个函数接受一个字符
# 向量和一个正则表达式，并显示出它们是如何匹配的。我们先从非常简单的正则表达式开
# 始，然后循序渐进地学习更加复杂的正则表达式。一旦掌握了模式匹配，你就知道如何将
# 这种思想应用于不同的stringr 函数了。
# 基础匹配
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")

# 要想建立正则表示式，我们需要使用\\
dot <- "\\."
# 实际上表达式本身只包含一个\：
writeLines(dot)

# 这个表达式告诉R搜索一个.
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")

# 描点
# 默认情况下，正则表达式会匹配字符串的任意部分。有时我们需要在正则表达式中设置锚
# 点，以便R 从字符串的开头或末尾进行匹配。我们可以设置两种锚点。
# ??? ^ 从字符串开头进行匹配。
# ??? $ 从字符串末尾进行匹配。
# 始于权力^，终于金钱$

x <- c("apple","banana","pear")
str_view(x, "^a")
str_view(x, "a$")

# 强制正则表达式进行匹配一个完整的字符串
x <- c("apple pie", "apple", "apple cake")
str_view(x,"apple")
str_view(x, "^apple$")

# 字符类和字符选项
# 很多特殊模式可以匹配多个字符。我们已经介绍过.，它可以匹配除换行符外的任意字符。
# 还有其他4 种常用的字符类。

# \d 可以匹配任意数字。
# \s 可以匹配任意空白字符（如空格、制表符和换行符）。
# [abc] 可以匹配a、b 或c。
# [^abc] 可以匹配除a、b、c 外的任意字符。

str_view(c("grey","gray"), "gr(e|a)y")

# 正则表达式的另外一个功能是：重复
# 正则表达式的另一项强大功能是，其可以控制一个模式能够匹配多少次。
# ?：0 次或1 次。
# +：1 次或多次。
# *：0 次或多次。
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "CC*")
str_view(x, "C[LX]+")

# 你还可以精确设置匹配的次数。
# ??? {n}：匹配n 次。
# ??? {n,}：匹配n 次或更多次。
# ??? {,m}：最多匹配m 次。
# ??? {n, m}：匹配n 到m 次。
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,2}")
str_view(x, "C{2,3}")

# 默认的匹配方式是“贪婪的”：正则表达式会匹配尽量长的字符串。通过在正则表达式后
# 面添加一个?，你可以将匹配方式更改为“懒惰的”，即匹配尽量短的字符串。虽然这是正
# 则表达式的高级特性，但知道这一点是非常有用的。
str_view(x, "C{2,3}?")
str_view(x, "C[LX]+")

# 分组与回溯引用
str_view(fruit, "(..)\\1", match = TRUE)
fruit

# 使用stringr处理字符串
# 使用forcats处理因子



# 使用lubridate处理日期和时间
# 加载包
library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()

# 通过字符串创建
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd(20170131)

# ymd() 和类似的其他函数可以创建日期。要想创建日期时间型数据，可以在后面加一个下
# 划线，以及h、m 和s 之中的一个或多个字母，这样就可以得到解析日期时间的函数了：
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC")

# 通过各个成分构建
# 除了单个字符串，日期时间数据的各个成分还经常分布在表格的多个列中。航班数据就是
# 这样的：
flights %>% 
  select(year, month, day, hour, minute) 

# 如果想要按照这种表示方法来创建日期或时间，可以使用make_date() 函数创建日期，使
# 用make_datetime() 函数创建日期时间：
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure=make_datetime(year, month, day, hour, minute))

# 我们对flights 中的4 个时间列进行相同的操作。因为时间的表示方法有点奇怪，所以我
# 们先使用模运算将小时成分与分钟成分分离。一旦建立了日期时间变量，我们就在本章剩
# 余部分使用这些变量进行讨论：
make_datetime_100 <- function(year,month,day,time){
  make_datetime(year, month, day, time %/% 100, time %% 10)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time=make_datetime_100(year, month, day, dep_time),
    arr_time=make_datetime_100(year, month, day, arr_time),
    sched_dep_time=make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time=make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

# 可以这些数据做出一年间做出出发时间的分布
flights_dt %>% 
  ggplot(aes(dep_time))+
  geom_freqpoly(binwidth=86400)

# 或者是一天内的分布
flights_dt %>% 
  filter(dep_time<ymd(20130102)) %>% 
  ggplot(aes(dep_time))+
  geom_freqpoly(binwidth=600)

# 通过其他类型的数据创建
# 在日期时间型数据和日期型数据之间进行转换，这正是as_datetime() 和as_date() 函数的功能：
as_datetime(today())
as_date(now())

# 有时我们会使用“Unix 时间戳”（即1970-01-01）的偏移量来表示日期时间。如果偏移
# 量单位是秒，那么就使用as_datetime() 函数来转换；如果偏移量单位是天，则使用as_
# date() 函数来转换：
as_datetime(60*60*10)
as_date(365*10+2)

# 时间日期成分

# 获取成分
# year()、month()、mday()（一个月中的第几天）、yday()（一年中的第几天）、wday()（一周中的第几天）、
# hour()、minute() 和second()
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
day(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

# 对于month() 和wday() 函数，你可以设置label = TRUE 来返回月份名称和星期数的缩写，
# 还可以设置abbr = FALSE 来返回全名：
month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = TRUE)

# 通过wday() 函数，我们可以知道在工作日出发的航班要多于周末出发的航班：
flights_dt %>% 
  mutate(wday=wday(dep_time,label = TRUE)) %>% 
  ggplot(aes(x=wday))+
  geom_bar()

# 查看一小时内每分钟的平均出发延误
flights_dt %>% 
  mutate(minute=minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarize(
    avg_delay=mean(arr_delay, na.rm = TRUE),
    n=n()
  ) %>% 
  ggplot(aes(minute, avg_delay))+
  geom_line()

# 在第20~30 分钟和第50~60 分钟内出发的航班的延误时间远远低于其他时间出发的航班！

# 如果检查计划出发时间，我们就会发现其中没有这么明显的模式
sched_dep <- flights_dt %>% 
  mutate(minute=minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarize(
    avg_delay=mean(arr_delay, na.rm = TRUE),
    n=n()
  ) 

ggplot(sched_dep, aes(minute, avg_delay))+
  geom_line()

# sched_dep <- flights_dt %>%
#   mutate(minute = minute(sched_dep_time)) %>%
#   group_by(minute) %>%
#   summarize(
#     avg_delay = mean(arr_delay, na.rm = TRUE),
#     n = n())
# ggplot(sched_dep, aes(minute, avg_delay)) +
#   geom_line()
ggplot(sched_dep, aes(minute, n))+
  geom_line()

# 舍入
# 绘制独立日期成分的另一种方法是，通过floor_date()、round_date() 和ceiling_date()
# 函数将日期舍入到临近的一个时间单位。这些函数的参数都包括一个待调整的时间向量，
# 以及时间单位名称，函数会将这个向量舍下、入上或四舍五入到这个时间单位。
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line()

# 设置成分
(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
datetime
month(datetime) <- 08
datetime
hour(datetime) <- hour(datetime)+1

# 使用update() 函数来显示这一年中所有航班的出发时间在一天内的分布：
flights_dt %>% 
  mutate(dep_hour=update(dep_time, yday=1)) %>% 
  ggplot(aes(dep_hour))+
  geom_freqpoly(binwidth=300)

# 时间间隔
# 时期
h_age <- today()-ymd(19791014)
h_age

as.duration(h_age)
dseconds(15)
dminutes(10)
dhours(10)
dhours(c(10,20))
ddays(0:5)
dweeks(3)
dyears(1)

2*dyears(1)
dyears(1)+dweeks(12)+dhours(15)
tomorrow <- today()+ddays(1)
last_year <- today()-dyears(1)
tomorrow
last_year

one_pm <- ymd_hms(
  "2016-03-12 13:00:00",
  tz="America/New_York"
)

one_pm
one_pm+ddays(1)

# 阶段
seconds(15)
minutes(10)
hours(c(12,24))
days(7)
months(1:6)
weeks(3)
years(1)

# 可以对阶段进行乘法和加法操作
10*(months(6)+day(1))
days(50)+hours(25)+minutes(2)

# 闰年
ymd("2016-01-01")+dyears(1)
ymd("2016-01-01")+years(1)

# 夏时制
one_pm+ddays(1)
one_pm+days(1)

# 使用阶段来解决与航班日期有关的一个怪现象
flights_dt %>% 
  filter(arr_time < dep_time)

# 这些都是过夜航班。我们使用了同一种日期来表示出发时间和到达时间，但这些航班是在
# 第二天到达的。将每个过夜航班的到达时间加上一个days(1)，就可以解决这个问题了：
flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight*1),
    sched_arr_time = sched_arr_time + days(overnight*1)
  )

# 这样一来，航班数据就符合常理了
flights_dt %>% 
  filter(overnight, arr_time < dep_time)

# 区间
years(1)/days(1)
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
(today() %--% next_year) %/% days(1)

# 时区
# 在R 中，可以使用Sys.timezone() 函数找出你的当前时区：
Sys.timezone()
length(OlsonNames())
lead(OlsonNames())

# 在R 中，时区是日期时间型数据的一个属性，仅用于控制输出。
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1-x2
x1-x3

# 除非使用了其他设置，lubridate 总是使用UTC（Coordinated Universal Time，国际标准时
# 间）。UTC 是科技界使用的时区标准，基本等价于它的前身GMT（Greenwich Mean Time，
# 格林尼治标准时间）。因为没有夏时制，所以它非常适合计算。涉及日期时间的操作（比
# 如c()）经常会丢弃时区信息。在这种情况下，日期时间会显示你的本地时区：
x4 <- c(x1,x2,x3)
x4

# 保持时间不变，修改其显示方式。当时间正确，但需要更直观的表示时，可以使用这种
# 方法：
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4

# 修改内部时间。当时间数据被标注了错误的时区，而想要改正过来时，你就可以使用这
# 种方法：
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4

# 编程

# 使用magrittr进行管道操作
# 简介
# 准备工作
library(magrittr)

# 一只小兔叫福福
# 蹦蹦跳跳过森林
# 抓起一窝小田鼠
# 每只头上打一下
# foo_foo <- little_bunny()
# library(magrittr)
# foo_foo <- little_bunny()

# foo_foo_1 <- hop(foo_foo, through = forest)
# foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
# foo_foo_3 <- bop(foo_foo_2, on = head)


diamonds1 <- ggplot2::diamonds
diamonds2 <- ggplot2::diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)

# install.packages("pryr")
library(pryr)
# 查看占用的内存
pryr::object_size(diamonds1)
pryr::object_size(diamonds2)
pryr::object_size(diamonds1, diamonds2)

# 函数pryr::object_size() 会返回其所有参数占用的内存
# diamonds2 和diamonds 有10 个公共的数据列，这些列中的数据没必要再
# 复制一份，因此这两个数据框中有公用变量。公用变量只有在修改时才会进行复制
diamonds1$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds1, diamonds2)

# 重写初始对象
# foo_foo <- hop(foo_foo, through = forest)
# foo_foo <- scoop(foo_foo, up = field_mice)
# foo_foo <- bop(foo_foo, on = head)

# 函数组合
# bop(
#   scoop(
#     hop(foo_foo, through = forest),
#     up = field_mice
#   ),
#   on = head
# )

# 使用管道
# foo_foo %>%
#   hop(through = forest) %>%
#   scoop(up = field_mouse) %>%
#   bop(on = head)

# 管道的工作原理就是进行“词法变换”。在这种方式背后，magrittr 会重新组合管道代码，
# 按照重写中间变量的方式来运行
# 这意味着管道不能支持以下两类函数。
# assign() 函数会在当前环境中使用给定名称创建一个新
# 变量：
assign("x",10)
x
"x" %>% assign(100)
x

# 如果要通过管道方式来使用assign()，就必须显式地指定环境：
env <- environment()
"x" %>% assign(100, envir = env)
x

# 具有这个问题的其他函数包括get() 和load()
# 使用惰性求值的函数。在R 中，不会在函数调用前计算这种函数的参数，只在函数使
# 用时才进行计算。管道依次计算每个参数，因此不能用在这种函数上。
tryCatch(stop("!"), error = function(e) "An error")
stop("!") %>%
  tryCatch(error = function(e) "An error")
# 使用惰性求值的函数还是很多的，其中包括R 基础包中的try()、suppressMessages()
# 和suppressWarnings()。

# 不适合用管道函数的情景
# 管道是一种功能强大的工具，但并不是你的唯一选择，也不是“万能药”。管道最大的
# 用武之地是重写一段较短的线性操作序列。对于以下几种情形，我们认为最好不要使用
# 管道。
# ??? 操作步骤超过10（参考值）个。这种情况下，应该使用有意义的变量来保存中间结果。
# 这样会使得调试更加容易，因为你更容易检查中间结果；还可以使得代码更容易理解，
# 因为有意义的变量名称可以帮助别人明白你的代码意图。
# ??? 有多个输入和输出。如果需要处理的不是一个基本对象，而是组合在一起的两个或多个
# 对象，就不要使用管道。
# ??? 操作步骤构成一张具有复杂依赖关系的有向图。管道基本上是一种线性操作，如果使用
# 它来表示复杂的关系，通常会使得代码混乱不清。

# magrittr中的其他工具
# tidyverse 中的所有包都会自动加载%>%，因此一般不用显式加载magrittr。然而，magrittr
# 包中还有其他一些有用的工具，你或许想要尝试一下。

# ??? 在使用比较复杂的管道操作时，有时会因为某个函数的副作用而调用它。比如，你可能
# 想要打印或绘制出当前对象，或者想将它保存在硬盘中。很多时候这种函数不会返回任
# 何结果，只会有效地结束管道操作。

# 为了解决这个问题，你可以使用“T”管道操作符%T>%。它的用法和%>% 差不多，只是
# 它返回的是左侧项而不是右侧项。之所以称它为“T”操作符，是因为它起的作用类似
# 于T 形三通管道：

rnorm(100) %>% 
  matrix(ncol = 2) %>% 
  plot() %>% 
  str()

rnorm(100) %>% 
  matrix(ncol=2) %T>% 
  plot() %>% 
  str()

# 如果使用的函数不是基于数据框的（也就是说，你必须传给这些函数一个独立的向量，
# 不能传给它们数据框或基于数据框求值的表达式），那么你就会发现爆炸操作符%$% 的
# 妙处。它可以将数据框中的变量“炸出来”，让你显式地引用。当需要使用R 基础包中
# 的很多函数时，这个操作符特别奏效：
mtcars %$% cor(disp, mpg)

mtcars <- mtcars %>% transform(cyl=cyl*2)
  




