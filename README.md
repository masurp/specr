
# specr initial glmer implementation 


Intended usage is a follows: 

```
my_glmer <- function(formula,data){
  glmer(formula, data = data, family = binomial)
}


results <- run_specs(df = example_data, 
                     y = c("y1","y2"),
                     x = c("x1","x2"),
                     model = c('my_glmer'), 
                     random_var_components = c("(1|group1)","(x1|group1)"),
                     controls = c("c1","c2"), 
                     subsets = list(group1 = unique(example_data$group1)))
                     )
```

Hope it's helpful. Thanks for the great package!
