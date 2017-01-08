
dataSource <- read.csv("~/GeneticAlgorithm/tests/testthat/jialing.csv", header = TRUE)
dataCount <- nrow(dataSource); #染色體長度
budget <- 80; #設定金額
config <- list(
  iters           = 100,
  population.size = 5,
  chromosome.size = dataCount,
  mutate.rate     = 0.5
);

population.initial <- function(
) {
    # TODO : implement initialization function/interface
    result <- rep(0, config$chromosome.size, each = 1);
    idx <- sample(dataCount, 1);
    while(dataSource[idx, 3] > budget) {
        idx <- sample(dataCount, 1);
    }
    result[idx] <- 1;
    return (result);
}
#計算適應值(80-價位+滿意度*飽足感)
chromosome.fitness <- function(chromosome
) {
    # TODO : implement fintness function/interface
    chrom <- as.vector(chromosome);
    chromSize <- config$chromosome.size;
    values <- rep(budget, chromSize);
    for(i in 1:chromSize) {
        CS <- d[i, 4]; #個人滿意度
        ST <- d[i, 5]; #飽足感
        AMT <- d[i, 3]; # 金額
        FLAG <- chrom[i]; # 0 OR 1
        values[i] <- values[i] + FLAG * (CS * ST - AMT);
    }
    return (values);
}
#
population.selection <- function(dataset,
                                 iters,
                                 current=NA,
                                 callback
) {
  # TODO : implement selection function/interface
  c1 <- NA;
  c2 <- NA;
  bestObj <- getBestObj(current, function(chrom) {
    c1 <<- chrom;
  });
  getBestObj(dataset, function(chrom) {
    c2 <<- chrom;
  });
  #
  if (is.function(callback)){
    callback(bestObj);
  }
  rbind(c1, c2);
}

#交配(使用多點交配，交配兩次)
chromosome.crossover <- function(dataset
) {
    # TODO : implement crossover function/interface
    parents <- dataset;
    rows <- nrow(parents);
    cols <- ncol(parents);
    # 兩兩交配
    e <- as.integer(rows/2) * 2;
    for (i in seq(1, e, 2)) {
        c1 <- parents[i, ];
        c2 <- parents[i+1, ];
        #交配兩次
        for (j in 1:2) {
            r <- sample(cols, 2);
            temp <- c1[r[1]];
            c1[r[1]] <- c2[r[2]];
            c2[r[2]] <- temp;
        }
    }
    r <- sample(rows, 1);
    if (r==rows && rows %% 2 !=0) {
        r <- r-1;
    }
    return (parents[r, ]);
}

#突變(隨機選擇一個數字，再依機率選擇要不要突變)
chromosome.mutation <- function(chromosome,
                                iters
) {
  # TODO : implement mutation function/interface
  mutate.rate <- config$mutate.rate;
  chrom <- as.vector(chromosome);
  if (runif(1,0,1) < mutate.rate || iters %% 2==0) {
    cols <- length(chrom);
    count <- as.integer(cols / 2);
    seatNum <- as.integer(dataSource$seat / cols);
    for (i in 1:count) {
      p <- sample(1:cols, size=1);
      n <- sample(0:1, size=1);
      x <- ifelse(n==0, 1, -1);
      m <- sample(0:seatNum, size=1);
      chrom[p] <- abs(chrom[p] + x * m) # %% seat;
    }
  }
  chrom
}
#
onFinalize <-function(dataset,
                      lastPopulation=NA,
                      callback=NA
) {
  # TODO : implement finalize function/interface
  if (is.function(callback)){
    bestObj <- getBestObj(lastPopulation);
    callback(bestObj);
  }
}
#
getBestObj <- function(dataset, callback=NA) {
  idx <- which.max(dataset[, 1]);
  bestObj <- dataset[idx, ];
  if (is.function(callback)){
    chrom <- gcga.getChromosome(bestObj);
    callback(chrom);
  }
  return(bestObj);
}

GAmodel <- gcga(iters = config$iters,
                population.size = config$population.size,
                chromosome.size = config$chromosome.size,
                onInit          = population.initial,
                onFitness       = chromosome.fitness,
                onSelection     = population.selection,
                onCrossover     = chromosome.crossover,
                onMutation      = chromosome.mutation,
                onFinalize      = onFinalize);
# p <- 1;
# for (i in 1:GAmodel$iters){
#   population <- GAmodel$dataset$Generation[[i]];
#   for (j in 1:GAmodel$population.size) {
#     chrom <- population[j, 2:GAmodel$chromosome.size];
#     value <- population[j, 1];
#     strChrom <- paste(chrom, collapse = ',')
#     print(sprintf("population:%d, value:%d, chromosome:%s", p, value, strChrom));
#     p <- p + 1;
#   }
# }

# draw it!
FV <- as.integer(GAmodel$dataset$Elitism[, 1] / 10000);
GN <- c(1:GAmodel$iters);
AGE<- as.factor(as.integer(GN/100)+1);
DS <- data.frame(cbind(FV, GN));
ggplot() +
  # Scatter Diagram(Scatter Plot)
  geom_point(aes(x=DS$GN, y=DS$FV, color=AGE)
  ) +
  # Trendline
  geom_smooth(aes(x=DS$GN, y=DS$FV)
  ) +
  # Annotation
  labs(title="Gordon's Scatter of Genetic Algorithm",
       x="Generation",
       y="FV (million)"
  ) +
  # background white: http://docs.ggplot2.org/current/ggtheme.html
  theme_bw();

#
# TODO : UNIT TEST
#
#expect_equal(GAmodel$iters, config$iters)





