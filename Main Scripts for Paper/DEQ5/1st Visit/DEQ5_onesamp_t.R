##DEQ one-sample T. Test##

res_Drug <- t.test(D1$Drug_Effect, mu = 0)
res_High<- t.test(D1$High, mu = 0)
res_Like <- t.test(D1$Like, mu = 0)
res_Dislike <- t.test(D1$Dislike, mu = 0)
res_Want <- t.test(D1$Want_More, mu = 0)

print(res_Drug)
print(res_High)
print(res_Like)
print(res_Dislike)
print(res_Want)