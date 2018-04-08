library(magrittr)
library(dplyr)
library(ggplot2)
library(magick)

#設定工作路徑，不過待會應該用不到
setwd("C:\\RData")

#支援哪些功能與格式
str(magick::magick_config())

#讀取圖片(網址或路徑)
finn <- image_read('C:/Users/yeyut/Pictures/Adventure Time/finn.jpg')
BMO <- image_read('https://i.imgur.com/tyzDiJc.png')

#圖片資訊
#Q 資訊裡的matte是什麼?
image_info(finn)
image_info(BMO)

#圖片匯出，format有點沒用，最後輸出是看檔名而定
image_write(BMO, path = "BMO.jpg", format = "png")

#圖片格式轉換(jpg轉png, jpg跟jpeg是一樣的格式)
image_types()
finn.png <- image_convert(finn,"png")
BMO.jpg <- image_convert(BMO,"JPEG")
image_info(finn.png)
image_info(BMO.jpg)

#需要X11(X Window系统)
image_display(BMO)
#用本機內建瀏覽器開啟
image_browse(BMO.jpg)

#背景裁切，隨fuzz值會自動選取適當範圍
BMO
image_trim(BMO,fuzz = 20)

#背景裁切，選擇要裁切的畫素大小
#Q 右下兩邊裁減不掉
image_chop(BMO,"120X50")

#圖片裁切，寬X高+起點x值+終點y值
##X或x大小寫都可以
#Q repage功能不明
image_crop(BMO.jpg, "700x700+100+50")


#調整畫素尺度，寬(或高)會隨著等比例縮放
##畫素比原圖片大則會模糊
##寬變400畫素
image_scale(BMO.crop, "400")
##高變500畫素
image_scale(BMO.crop, "X500")
##寬不超過400，並且高不超過500
image_scale(BMO.crop, "400X500")
##寬等於400，高等於500，圖片會變形
image_scale(BMO.crop, "400X500!")


#調整畫素尺度，跟scale差不多
##放大圖片時filter可以改變模糊效果，預設是Triangle
filter_types()
rose <- image_convert(image_read("rose:"), "png")
image_resize(rose, "700X400")
image_resize(rose, "700X400", filter = "Triangle")
image_resize(rose, "700X400", filter = "Point")
image_resize(rose, "700X400", filter = "Box")
image_resize(rose, "700X400", filter = "Sinc")


#顏色填滿，點位置point = "+X+Y"，fuzz設定模糊範圍
#Q 查詢點像素位置，我想把牙齒變金色，慢慢找點很辛苦
image_fill(BMO, "gold","+390+400",fuzz = 60)

#加入邊框，顏色可選，寬X高
image_border(BMO, "gold", "10x20")

#填充背景顏色，但無法覆蓋原背景
image_background(BMO,"gold")

snail <- image_read("https://cs5.pikabu.ru/images/big_size_comm/2014-07_1/14042186755412.png")
snail
image_background(snail, "gold")

#旋轉，上下翻轉，左右翻轉
BMO
image_rotate(BMO, 30)
image_flip(BMO)
image_flop(BMO)
#guess what
BMO %>% image_flip %>% image_flop %>% 
        image_rotate(180)

#調節亮度、飽和度、色相，原圖為100(猜是百分比)

#亮度(brightness)，負無限到正無限，小於0皆為黑色
image_modulate(BMO,brightness = 120,
               saturation = 100,hue = 100)
#飽和度(saturation)，0則為灰階,可以負值
image_modulate(BMO,brightness = 100,
               saturation = 200,hue = 100)
image_modulate(BMO,brightness = 100,
               saturation = 0,hue = 100)
image_modulate(BMO,brightness = 100,
               saturation = -100,hue = 100)
#色相(hue)，非度數，hen奇怪
image_modulate(BMO,brightness = 100,
               saturation = 100,hue = 90)

#高斯模糊，radius跟sigma都要設值才有效果
#Q 能不能部分模糊，比如臉部打馬賽克
#A 學到image_composite就能自己寫局部模糊函數
image_blur(BMO,radius = 10,sigma = 3)



#局部模糊，geometry = 寬X高+從左先裁掉+從上先裁掉
##用兩張一樣的圖，第一張剪裁模糊，再貼到第二張相同位置
##R可能不太適合處理svg，會失去svg可縮放的本質
part_blur <- function(image.magick, geometry){
      #format.info <- image_info(image.magick)$format
      image.magick %>% 
      #image_convert(format = "PNG") %>% 
      image_crop(geometry = geometry) %>% 
      image_blur(radius = 15, sigma = 20) %>% 
      image_composite(image = image.magick, 
                      composite_image = .,
                      operator = "over",
                      offset = geometry) %>% 
      #image_convert(format = format.info) %>% 
      return()
    }

#把BMO的臉給模糊掉
part_blur(BMO,"370X300+250+170")  

#局部馬賽克效果
part_mosaic <- function(image.magick, geometry){
    #format.info <- image_info(image.magick)$format
    image.magick %>% 
    #image_convert(format = "PNG") %>% 
    image_crop(geometry = geometry) %>% 
    image_scale(image = ., geometry = 
                paste(0.1*(image_info(.)$width),"X",
                      0.1*(image_info(.)$height))) %>% 
    image_resize(image = ., geometry = 
                    paste(10*(image_info(.)$width),"X",
                          10*(image_info(.)$height)),
                 filter = "box") %>% 
    image_composite(image = image.magick, 
                    composite_image = .,
                    operator = "over",
                    offset = geometry) %>% 
    #image_convert(format = format.info) %>% 
    return()
}

#BMO臉上打馬賽克
part_mosaic(BMO,"370X300+250+170")
#svg圖檔也適用，跟我想像的不太一樣
kiwi <- image_read('http://www.webhek.com/wordpress/wp-content/uploads/2014/05/kiwi.svg')
part_mosaic(kiwi,"200X200+350+100")



#加入噪點，預設不知道是什麼
BMO
image_noise(BMO)
noise_types()
image_noise(BMO, noisetype = "Laplacian")
image_noise(BMO, noisetype = "gaussian")
image_noise(BMO, noisetype = "Multiplicative")
image_noise(BMO, noisetype = "poisson")
image_noise(BMO, noisetype = "impulse")


#圖案效果(拓印、油畫、負片、黑洞)
BMO
image_charcoal(BMO)
image_charcoal(BMO,radius = 5)
image_charcoal(BMO,radius = 5, sigma = 3)
image_oilpaint(BMO)
image_oilpaint(BMO,radius = 5)
image_negate(BMO)
image_implode(BMO)

#加入文字方塊，無法打中文欸!
##fonts:"mono", "Times", "Helvetica", "Trebuchet","Comic Sans".
gravity_types()
image_annotate(
    image = BMO, #圖片
    text = c("BMO the little boy !"), #註解內容
    size=50,       #文字大小
    degrees = -5,    #旋轉角度
    gravity = "south",  #設定座標原點，預設在"NorthWest"
    location = "-10+35",  #註解的座標位置
    color = "blue",  #文字顏色
    boxcolor = "lightgreen", #文字方塊背景顏色
    strokecolor = "black", #文字邊緣顏色
    font = "mono") #字型
gravity_types()


#這個我就不太懂了
kernel_types()
image_convolve(BMO,kernel = 'Ridges',scaling = "120%!",bias = "-5%")


#限制色彩數
##預設應該是用sRGB
colorspace_types()
image_quantize(BMO, max = 2) 
image_quantize(BMO, max = 3) 
image_quantize(BMO, max = 4)
image_quantize(BMO, max = 5)
image_quantize(BMO, max = 3,colorspace = 'gray')


#搭配piping
(BMO.light <- BMO %>% 
    image_crop("700x700+100+50") %>% 
    image_border("skyblue","10X10") %>% 
    image_fill("gold","+300+360",fuzz = 60) %>%
    image_fill("gold","+270+355",fuzz = 50) %>%
    image_scale("720") %>% 
    image_annotate(text = c("BMO the golden teeth"),
                   gravity = "south", size = 50,
                   location = "+0+50",
                   degrees = -5,
                   color = "gold") %>% 
    image_oilpaint(3.5))

(BMO.dark <- BMO %>% 
        image_crop("700x700+100+50") %>% 
        image_border("skyblue","10X10") %>% 
        image_fill("gold","+300+360",fuzz = 60) %>%
        image_fill("gold","+270+355",fuzz = 50) %>%
        image_scale("720") %>% 
        image_flop %>%
        image_annotate(text = c("BMO the blue teeth"),
                       gravity = "south", size = 50,
                       location = "+0+50",
                       degrees = 5,
                       color = "gold") %>%
        image_modulate(b = 110, s = -100) %>% 
        image_oilpaint(3.5))

#將兩張圖合併顯示，stack堆疊成直行
image_append(c(BMO.dark,BMO.light))
stack <- image_append(c(BMO.dark,BMO.light),stack = T)

        
        
#可以讀GIF檔案，有幾次assign之後畫素變小了
image_read("https://tctechcrunch2011.files.wordpress.com/2015/08/safe_image.gif")
kuat <- image_read("https://tctechcrunch2011.files.wordpress.com/2015/08/safe_image.gif")
length(kuat)
##GIF的畫素不能隨便調整
kuat %>% image_scale("500X500")
##動畫要處理比較久，畢竟140張圖都要處理
kuat %>% image_flop %>% 
    image_annotate("Run Forrest Run",
                   size = 40,
                   color = "blue",
                   gravity = "North")


#動畫，用c()連結數張靜圖形成動畫，但不能調速度
##以第一張圖的畫素作為顯示尺度，顯示順序受限
pic1 <- image_read('https://orig00.deviantart.net/cf11/f/2012/220/4/6/adventure_time_tree_house_by_transparentstuff-d5adrao.png') %>% 
         image_scale("720")
pic2 <- image_read("http://3rd-strike.com/wp-content/uploads/2015/02/Adventure-Time.png")
pic3 <- image_read("https://vignette.wikia.nocookie.net/universe-of-smash-bros-lawl/images/e/e9/Finn_and_Jake.png/revision/latest?cb=20160306001537") %>% 
         image_scale("X300")



#疊圖，將數張圖依次疊成一張靜圖，每張圖畫素不受影響
image_mosaic(c(pic1, pic2, pic3))
image_mosaic(c(pic3, pic1, pic2))

#疊圖，依次疊加，但顯示尺度以第一張圖的畫素為準
pic2
image_flatten(c(pic2, pic1, pic3))
pic3
image_flatten(c(pic3, pic1, pic2))

layer <- image_flatten(c(pic1, pic2, pic3))
compose_types()

#一個疊圖順便套濾鏡的概念吧我猜
image_flatten(layer, operator = 'MinusSrc')
image_flatten(layer, operator = 'HardLight')
image_flatten(layer, operator = 'Colorize')
compose_types()

#疊圖，offset可調整位置，operator可調整疊圖方式
compose_types()
image_composite(image = pic1,
                composite_image = pic3,
                operator = "over",
                offset = "+100+200")

#把三張圖疊圖，並適當調整位置、大小，裁切並輸出
##先將pic3旋轉、縮小，注意要先設定背景為none
pic3.mod <- pic3  %>%
        image_background("none") %>% 
        image_rotate(-10)  %>% 
        image_scale("350")
AT <- pic1 %>% image_composite(pic3.mod,
                         operator = "over",
                         offset = "+80+180") %>% 
    image_composite(image_scale(pic2,"500"),
                    operator = "over",
                    offset = "+0+20") %>% 
    
    image_crop("500X500") %>% 
    image_annotate(text = "Finn & Jack", 
                   gravity = "southeast",
                   size = 20,color = "black",
                   font = "Comic Sans",
                   location = "+15+10") %>%
    image_background("skyblue")
image_write(AT,"adventure time mix.png")

#製作動畫，fps調整速度，loop動畫重複次數
##dispose調整布置方式，一律小寫
c(pic1, pic2, pic3)
c(pic3, pic2, pic1)

dispose_types()
image_animate(image = c(pic1, pic2, image_border(pic3,"none","100X200")),
              fps = 2, dispose = "none")


#漸變效果，frams為漸變過程的圖片張數
frames <- image_morph(c(BMO.light, image_flop(BMO.dark),BMO.light), frames = 20)
length(frames)
frames[1]
frames[11]
frames[22]
#用animate調整動畫漸變速度，不知為何跑不太動
BMO.morph <- image_animate(frames,fps = 10,loop = 3)

#也可以將動畫跟靜圖用conposite疊圖，還是一樣超慢
##也可用image_apply將動畫每張圖依次composite
jake.gif <- image_read('https://vignette.wikia.nocookie.net/horadeaventura/images/7/79/FlameDarkKingFirma2.gif/revision/latest?cb=20160322175908&path-prefix=es')
jake.gif2 <- image_read('https://media.giphy.com/media/G9AjdAf3cGmfS/giphy.gif') %>% 
               image_crop("220X200+200+30")

AT %>% image_composite(image_scale(jake.gif,"100"),
                       operator = "over",
                       offset = "+150+0") %>% 
       image_animate(fps = 10,loop = 3)

#利用graph將圖表輸出到Viewr，再加入靜圖或動畫
##graph打開圖形裝置介面，跑完圖表後要關閉裝置
figure <- image_graph(width = 700, height = 700, res = 96)
ggplot2::qplot(carat,price, data = diamonds[sample(1:50000,1000),], colour = color)
dev.off()

mark <- pic3 %>% image_scale("150X150")
image_composite(figure, mark, offset = "+450+500")

mark2 <- jake.gif2 %>% image_scale("150X150")
myfig <- image_composite(figure, mark2, offset = "+450+500") %>% 
    image_animate(fps = 10,dispose = "none")


#用draw開啟就能用低階繪圖函數進行後製
img <- image_draw(myfig)
#rect逆時針左下右上
rect(80, 650, 240, 500, border = "red", lty = "dashed", lwd = 5)
#Q abline能不能設邊界
plot(cars)
abline(h = 60, col = 'red',lwd = '2', lty = "dotted")
#加入文字,x、y設定文字方塊的寬跟高要多長
text(x = 30,y =  250, "Hoiven-Glaven", family = "monospace", cex = 4, srt = 90)
#也能用annotation加入文字
image_annotate(img, "XDDDDD",size = 100)
dev.off()

#讀取pdf失敗，原因不明
mypdf <- image_read('https://ir.nctu.edu.tw/bitstream/11536/71395/1/342302.pdf')
mypdf <- image_read('C:/RData/magick.pdf')

#改用pdftools，但一次只能讀一頁，無法知道總頁數
library(pdftools)
pdf2bmp <- pdf_render_page('https://cran.r-project.org/web/packages/magick/magick.pdf',
                           page = 5)
#解析後為點陣圖(bitmap,bmp)
class(pdf2bmp)
#可以用image_read讀取bmp檔
bmp2png <- image_read(pdf2bmp)


#各國人均GDP資料
library(gapminder)
View(gapminder)
#將資料按照年份進行分割
data.list <- split(gapminder, gapminder$year)
#用ggplot的作圖輸出到Viewer
img <- image_graph(600, 340, res = 96)
#畫人均GDP對平均壽命的散佈圖，點大小根據人口數變化
##用顏色區分不同洲，標題則是分割資料的年分
data.list %>% lapply(function(data){
    ggplot(data, aes(gdpPercap, lifeExp, 
             size = pop, color = continent)) +
           scale_size("populati0n", 
             limits = range(gapminder$pop)) + 
           geom_point() + ylim(20, 90) + 
           scale_x_log10(limits = range(gapminder$gdpPercap)) +
           ggtitle(data$year) + 
           theme_classic()
})
dev.off()
(animation <- image_animate(img, fps = 2))
animation.at <- image_composite(animation,image_scale(pic3,"100"),
                offset = "+380+200")
image_write(animation.at,"gdp.gif")


#R內建Raster(點陣圖)套件，但仍要用image_read讀取圖檔
#Q 有沒有內建讀取圖檔的函數
frog <- image_read('https://media.kocpc.com.tw/kocpc/2018/01/1517192408-1d049705f439174de7fe71ecffbaeb79.jpg')
frog 
class(frog)
#將magick-image變數轉成raster變數
frog.ras <- as.raster(frog)
frog.ras

class(frog.ras)
#raster為矩陣，plot可以畫raster
## magick-image繪畫再Viewer，plot則是畫在plots
plot(frog.ras)
#locator(n = 4,type = "n") #在R裝置介面下選4點，便回傳每個座標位置
#便可用低階繪圖函數處理圖片
rect(11, 15, 189, 189, border = "red", lty = "dashed", lwd = 5)


#rasterImage可以當作是低階繪圖函數，對圖表進行加工
##locator能找出plots圖表中的點座標(對應橫縱軸數值)
plot(iris$Sepal.Length) 
locator(n = 4,type = "n")
rasterImage(frog, 0, 6, 29, 8)

#grid套件中有grid.raster也是低階繪圖函數
##這個函數位置很不好抓，比rasterImage還不方便
library(grid)
hist(iris$Sepal.Length)
grid.raster(frog,x = 0.82, y = 0.68,
            width = 0.16, height = 0.25)


#利用內建raster套件中的brick創建RasterBrick圖形變數
##再透過plotRGB輸出圖檔，輸出能固定寬高比並填滿視窗

#brick能創建RasterBrick變數，這跟SpatialPolygon有點像
##兩種變數都屬於S4 class，包含數個slots可用@呼叫
###我目前試過brick能讀的只有圖檔路徑、bitmap變數
library(raster)
BMO.rb <- raster::brick("C:/Users/yeyut/Pictures/Adventure Time/BMO the little boy II.png")
class(BMO.rb)
str(BMO.rb)
raster::plotRGB(BMO.rb)

#不能直接brick網址，要先把網址的圖檔載下或從R匯出
#gunter.rb <- brick("https://vignette.wikia.nocookie.net/adventuretimewithfinnandjake/images/2/22/Gunter_in_Sign.png/revision/latest?cb=20121115114124")
gunter <- image_read("https://vignette.wikia.nocookie.net/adventuretimewithfinnandjake/images/2/22/Gunter_in_Sign.png/revision/latest?cb=20121115114124")
gunter
#不用設檔案格式(jpg,png,tiff)，brick讀得出來
image_write(gunter,"C:\\RData\\gunter")
gunter.rb <- brick("C:\\RData\\gunter")
#跟上面gunter比較一下
plotRGB(gunter.rb)


#tempfile()是一個路徑,tiff是點陣圖格式之一
tempfile()
tiff.file <- tempfile()
#將圖檔寫到佔存路徑，檔案格式為tiff，似乎是很彈性的格式
image_write(frog, path = tiff.file, format = 'tiff')
rb <- raster::brick(tiff.file)
class(rb)
str(rb)
#用plotRGB畫RasterBrick變數
##圖形寬高比例不會跑掉，會隨著視窗大小縮放
plotRGB(rb)


##magick.image變數frog[[1]]偷藏著bitmap資訊
frog.bmp <- frog[[1]]
class(frog.bmp)
#將bitmap的點陣圖資訊轉成數字陣列(多維的像素矩陣)
frog.array <- as.integer(frog.bmp)
class(frog.array)

#brick不能用bitmap變數創建圖檔，卻可以用array創建
rb <- brick(frog.bmp)
rb <- brick(frog.array)
#asp=高/寬
plotRGB(rb, asp = 1)

#光學字符辨識Optical Character Recognition,OCR
words.image <- image_read("http://jeroen.github.io/images/testocr.png")
words.image
class(words.image)
words.char <- magick::image_ocr(words.image)
class(words.char)
words.char
cat(word.chr)

#image_ocr功能猜是來自套件tesseract中ocr
library(tesseract)
tesseract::ocr(words.image) == magick::image_ocr(words.image)
#下載繁中跟簡中字庫，中文字辨識跑不了再回頭下載
tesseract_download("chi_tra", datapath = NULL, progress = TRUE)
tesseract_download("chi_sim", datapath = NULL, progress = TRUE)

#中文字辨識，錯字不少
words2 <- image_read('http://farm4.static.flickr.com/3207/3009617589_7be5462002_o.jpg')
words2
word2.chr <- image_ocr(words2,language = 'chi_tra')
cat(word2.chr)


#法文似乎在預設語言中
words3 <- image_read('https://admin.mashable.com/wp-content/uploads/2012/08/Obama-French.jpg') 
words3
image_ocr(words3) %>% cat

#泰文需要先下載字庫，R居然連泰文都能顯示
image_read('https://gss1.bdstatic.com/9vo3dSag_xI4khGkpoWK1HF6hhy/baike/s%3D220/sign=918ffece7a0e0cf3a4f749f93a44f23d/a044ad345982b2b7d17f6a1839adcbef77099b0d.jpg') %>% 
    image_ocr(language = "tha") %>% cat
#下載泰文↓
#tesseract_download("tha", datapath = NULL, progress = TRUE)


#文字呈圓形排列，完全不能辨識
image_read('https://lh6.googleusercontent.com/-uDSzGigVtMw/S1H1TsQCSyI/AAAAAAAAF1Y/TWHfup4e1RE/s450/2010011702.jpg') %>% 
    image_ocr(language = "chi_tra") %>% cat

#中英夾雜，中英可分開辨識，但中文錯字較多
image_read('http://img.mp.itc.cn/upload/20170513/26c9f2d4f59d493189d49ecb4a053ffd_th.jpg') %>% 
    image_ocr(language = "chi_sim") %>% cat

#辨識gunter寫字板失敗
gunter <- image_read("https://vignette.wikia.nocookie.net/adventuretimewithfinnandjake/images/2/22/Gunter_in_Sign.png/revision/latest?cb=20121115114124")
image_ocr(gunter)

#辨識pdf，dpi要夠高才能辨識的了，否則就是一堆wwwwwww
mypdf <- pdf_render_page('https://cran.r-project.org/web/packages/magick/magick.pdf',
       page = 1,dpi = 300) %>% image_read 
mypdf %>% image_ocr %>% cat

#辨識數統課本casella_berger_statistical_inference
##符號無法順利辨識，公式全亂了
stat.inf <- NULL
for(i in 30:32){
    stat.inf <- c(stat.inf,
    pdf_render_page('https://fsalamri.files.wordpress.com/2015/02/casella_berger_statistical_inference1.pdf',
     page = i,dpi = 800) %>% image_read %>% image_ocr)
}

#Q 中文有時候是直行書寫，甚至從右到左，可能無法解決
