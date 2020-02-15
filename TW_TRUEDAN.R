# 珍煮丹情人節買一送一活動
# 身分證同時包含 1、2、4 即符合資格

# 地區代碼 A => 10 , B => 11, C => 12  ....
areas_letter = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
                 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
                 'U', 'V', 'W', 'X', 'Y', 'Z')
areas = c(10, 11, 12, 13, 14, 15, 16, 17, 34, 18, 19, 20,
          21, 22, 35, 23, 24, 25, 26, 27, 28, 29, 32, 30,
          31, 33)

# 依據男女做計算
TW_ID <- function(gender) {
  number = paste(areas[area], gender, sep = '')
  number = paste(number, sprintf('%07d', i), sep = '')
  number = strsplit(toString(number), '')[[1]]
  number = as.integer(number)
  # 加上權數算總分
  for( j in 1:10 ) {
    if(j == 1) {
      score = number[j] * j
    } else {
      score = score + number[j] * (11 - j)
    }
  }
  # 產出驗證碼 （第 9 碼）
  verification = score %% 10
  # 身分證數字
  number[11] = verification
  return(number)
}

# 判斷是否包含欲判斷的數
PASS_ID <- function(one, two) {
  flag_one = 0
  flag_two = 0
  for( k in 4:11 ) {
    if(number[k] == one) {
      flag_one = flag_one + 1
    }
    if(number[k] == two) {
      flag_two = flag_two + 1
    }
  }
  if( flag_one > 0 && flag_two > 0 ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# 算出所有可能的組合
total = 0
pass = 0
for ( area in 1:26 ) { # id 地區
  for( i in 0:9999999 ) { # id 數字第 2 - 8 碼所有可能總數
    score = 0
    # 男生第一碼固定為 1
    number = TW_ID('1')
    print(number)
    # 檢查是否包含 2、4
    if( PASS_ID(2, 4) ) {
      pass = pass + 1
    }
    # 女生第一碼固定為 2
    number = TW_ID('2')
    print(number)
    # 檢查是否包含 1、4
    if( PASS_ID(1, 4) ) {
      pass = pass + 1
    }
    # 計算總數
    total = total + 2
  }
}

# 得到珍煮丹買一送一資格的機率
lucky = pass / total
# 沒得到珍煮丹買一送一資格的機率
unlucky = 1 - lucky