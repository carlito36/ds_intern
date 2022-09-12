#제가 phewas 원리를 알려고 임의로 넣은 데이터 입니다! 
#phewas 함수의 output은 phecode table인데 phecode가 1805개라 1805*n 의 형태로 나옵니다
sick = read.csv('sick_code.csv')
options(scipen=1000000)

#phenotype
sym <- data.frame(id,vocab,code,index)
code <- c(rep(c('A00','C00', 'A01.1'), 4))
id <- rep(c('18503', '33503', '18334425', '101'), each=3)
vocab <- rep('ICD10CM', 269844)
index <- c(1:269844)


pheno = createPhenotypes(id.vocab.code.index, min.code.count=2, add.phecode.exclusions=T, translate=T, id.sex, 
           full.population.ids=unique(id.vocab.code.index[[1]]),
           aggregate.fun=PheWAS:::default_code_agg, 
           vocabulary.map=PheWAS::phecode_map,
           rollup.map=PheWAS::phecode_rollup_map,
           exclusion.map=PheWAS::phecode_exclude)

#covariates
bmi <- c(21, 23, 24, 14)
age <- c(34, 35, 25, 60)
id <- c(18503, 33503, 18334425, 101)

#제 경우 pcos의 중복이환을 알고 싶었던 것이기 떄문에 
#outcome = pheno(이환정보) predictors = pcos 여부로 넣었습니다
caseyn <- rep(c(1,0,1), each=373)

#id를 키로 넣어주셔야 phewas에서 merge가 됩니다!
geno <- data.frame(health1['health.PERSON_ID'], caseyn)
cov <- data.frame(id, age, bmi)

#key 지정
names(pheno)[1] <- "KEY"
names(geno)[1] <- "KEY"
names(health1)[1] <- "KEY"

#phewas에는 id vocab code index 순으로 된 4개 컬럼의 데이터프레임을 outcome 에 넣어주셔야 합니다
result <- phewas(outcomes=pheno, predictors=geno, covariates=health1)


ggplot(result, aes(x=Variable, y=-log(p))) + geom_point(aes(col=predictor, size=OR)) + theme_classic() +theme(axis.text.x =element_blank(), panel.grid.minor=element_line(colour = "grey", linetype="dashed"), axis.ticks=element_blank()) + labs(color="Category", size="Effect size", x="GPS - Suicidal Phenotypes", y="log(p-value)") + geom_hline(yintercept=-log(0.01), color="red", size=1, alpha=0.5)

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             