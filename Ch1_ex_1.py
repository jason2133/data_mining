# 동전 바꾸기
# 그리디 알고리즘

def coinChange(money):
    change500 = money // 500
    money %= 500
    change100 = money // 100
    money %= 100
    change50 = money // 50
    money %= 50
    change10 = money // 10
    return change500, change100, change50, change10, money%10

if __name__ == '__main__':
    money = int(input("input money >> "))
    change = coinChange(money)
    print(f'{money}원은 500원 {change[0]}개, 100원 {change[1]}개, 50원 {change[2]}개, 10원 {change[3]}개이며 나머지는 {change[4]}원으로 교환됨')

