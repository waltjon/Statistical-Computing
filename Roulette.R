roulette = function(){
  #creates roulette wheel
  wheel = list(numbers = c(0:36), colors = c('green', rep(c('red', 'black'), 18)))
  
  #initializes money and gambling information
  money = 100; lose = 0; counter = 1;
  gambling = list(bets = c(0), amounts = c(0), win.lose = c(0), pocket = c(100));
  bet = 2^lose;
  
  #gambles and records
  while(money > (2^lose) && money < 110  && money > 0){
    
    bet = 2^lose;
    gambling$bets = append(gambling$bets, counter); counter = counter+1; gambling$bets
    gambling$amounts = append(gambling$amounts, bet);
    
    if(wheel$colors[sample(1:37, 1)] == 'red'){
      money = money + bet;
      gambling$win.lose = append(gambling$win.lose, 1);
      lose = 0;
    }else{
      money = money - bet;
      gambling$win.lose = append(gambling$win.lose, 0);
      lose = lose+1;
    }
    gambling$pocket = append(gambling$pocket, money);
  }
  return(gambling)
#end of function  
}