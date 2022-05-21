analyse_variance = function(s,p){
  
  m = matrix(s, ncol = p, byrow = TRUE)
  s_p = p*sum((apply(m, 1,mean) - mean(m))^2)
  v_p = s_p/(nrow(m)-1)
  s_i= nrow(m)*sum((apply(m,2,mean)-mean(m))^2)
  v_i = s_i/(p-1)
  s_t = sum((m-mean(m))^2)
  s_r = s_t - s_p - s_i
  v_r = s_r / ((nrow(m)-1)*(p-1))
  return(c(v_i/v_r, v_p/v_r))
  
  
}

prediction_moyenne = function(s,i) {
v = s[1:(i-1)]
return(mean(v))
}

eqm = function(prediction, verite){
return(mean((verite - prediction)^2))
}

prediction_k_valeur = function(s,i,k){
v = s[(i-k):(i-1)]
return(mean(v))
}

eval_k_valeur = function(s, k){
prediction = sapply(c((k+1):length(s)), function(x){prediction_k_valeur(huron$Niveau,x,k)})
resultat = eqm(prediction, s[k+1 : (length(s)-k)])
return (resultat)
}


prediction_lineaire = function(s,i){
 
 t = c(1:(length(s)-(length(s)-(i-1))))
 model = lm(s[1:(length(s)-(length(s)-(i-1)))] ~ t)
 data = data.frame("t"=i)
 prediction = predict(model, data)
 return(prediction) 
 
}


