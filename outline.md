### outline
* statistical background  
    - beta distribution  
    - reparameterization  
    - beinf with ones and zeros  
    - beinf [0,1] beta regression  
    - reduce it to ones *or* zeros  
* applying it to s&p psych  
    - likert/thermometer are continuously bounded  
    - can predict variance  
    - can address ceiling and/or floor effects  
* predicting variance  
    - norms  
    - empirical demonstration  
* ceiling and/or floor effects  
    - likening it to hurdle model  
    - captures relationship better than ols  
* implementation in gamlss
    - write a wrapper or no?

### notes
- maybe stick with inflated terminology before framing it as a hurdle model? otherwise, will need to explain what a hurdle model is, which ruins flow of explaining the distribution  
- so explain it as beinf, which is what gamlss uses, and then talk about it as a hurdle model, basing it off of the uses in econometrics  