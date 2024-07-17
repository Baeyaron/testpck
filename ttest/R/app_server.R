#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DT
#' @import car
#' @import datasets
#' @import stringr
#' @import nortest
#' @import shinydashboard
#' @import shinyjs
#' @noRd
app_server <- function(input, output) {
  options(shiny.maxRequestSize = 100 * 1024^2)

  scan <- function(string, sep) {
    re <- c()
    if (string == "") {
      return(re)
    } else {
      b <- strsplit(string, sep, fixed = T)
      for (i in 1:length(b[[1]]))
      {
        if (gregexpr(":", b[[1]][i])[[1]][1] == -1) {
          re_t <- as.numeric(b[[1]][i])
          re <- c(re, re_t)
        } else {
          cut <- gregexpr(":", b[[1]][i])[[1]][1]
          re_f <- as.numeric(substr(b[[1]][i], 1, cut - 1))
          re_b <- as.numeric(substr(b[[1]][i], cut + 1, nchar(b[[1]][i])))
          re_t <- re_f:re_b
          re <- c(re, re_t)
        }
      }
      return(re)
    }
  }

  # var view#
  var_view <- function(df) {
    sec <- rep(1:5, ceiling(ncol(df) / 5))[1:ncol(df)]
    re <- c()
    if (ncol(df) < 5) {
      for (i in 1:ncol(df))
      {
        re_t <- cbind(Num = i, Var = colnames(df)[i])
        re <- cbind(re, re_t)
      }
    } else {
      for (i in 1:5)
      {
        Var <- colnames(df)[sec == i]
        re_t <- cbind(Num = seq(i, ncol(df), 5), Var)
        if (length(seq(i, ncol(df), 5)) < ceiling(ncol(df) / 5)) {
          re_t <- rbind(re_t, matrix("", nrow = 1, ncol = 2))
        }
        re <- cbind(re, re_t)
      }
    }
    return(re)
  }

  nor_test <- function(df) {
    if (3 <= length(df) && length(df) <= 5000) {
      p <- shapiro.test(df)$p
    } else {
      p <- NA
    }
    return(p)
  }


  ####Tab Settings####
  ntabs <- 3
  tabnames <- c("setting", "results", "plots")
  tablabels <- c("Setting", "Download results", "Download plots")

  output$Sidebar <- renderUI({
    Menus <- vector("list", 2)
    Menus[[1]] <-   menuItem(tablabels[1], tabName = tabnames[1], icon = icon("gear"),selected = T)
    #    Menus[[2]] <-   menuItem(tablabels[2], tabName = tabnames[2], icon = icon("th"))
    #    Menus[[3]] <-   menuItem(tablabels[3], tabName = tabnames[3], icon = icon("dashboard"))

    Menus[[2]] <-   menuItem(c("Analysis and Download"), icon = icon("bar-chart-o"),
                             menuSubItem("Results and Download", tabName = "results"),
                             menuSubItem("Plots and Download ", tabName = "plots"))


    do.call(function(...) sidebarMenu(id = 'sidebarMenu', ...), Menus)
  })

  output$TABUI <- renderUI({
    Tabs <- vector("list", 3)
    Tabs[[1]] <- tabItem(tabName = tabnames[1],
                         fluidRow(
                           tabBox(
                             id = "method1",
                             title = "The type of t test",
                             side = "right", height = "300px",
                             selectInput("method",
                                         label= h3("Please choose"),
                                         choices = c("one sample"="one sample",
                                                     "paired samples"="paired samples",
                                                     "two independent samples"="two independent samples"),
                                         selected = 1),

                           ),


                           tabBox(
                             title = "Variable Settings",
                             side = "right", height = "300px",
                             textInput(inputId="variable",
                                       label="Analyzing variable (input the column number):"),
                             textInput(inputId = "group",
                                       label = "Grouping variable (input the column number):"),

                           )
                         ),

                         fluidRow(
                           tabBox(
                             title = "Population mean",
                             side = "right",width = 6,
                             textInput(inputId = "MEANS",
                                       label = "Population mean (one sample t test):")

                           )
                         ),
                         actionButton("act","Submit"),
                         h3(c("Column Numbers of Variables")),
                         DTOutput('view_var',width = "50%",height = "auto"))


    Tabs[[2]] <- tabItem("results",
                         h3(c("Results and Download")),
                         h3(c("Normality and homogeneity of variance (P values)")),
                         tableOutput(outputId ="view"),
                         h3("Two-sided statistics and P values"),
                         tableOutput(outputId ="view1"),
                         h3("One-sided P values"),
                         tableOutput(outputId ="view2"),
                         h4("The test statistic in the one-sided test is equal to that in the two-sided test"),
                         downloadButton(outputId ="downloadTable"),
                         width = 7)
    Tabs[[3]] <- tabItem("plots",
                         h3(c("Plots and Download")),
                         plotOutput(outputId = "boxPlot"),
                         downloadButton(outputId ="downloadPlot"),
                         width = 7)

    do.call(tabItems, Tabs)


  })


  observeEvent(input$file$datapath, {
    output$title <- renderText(c("Column numbers of variables"))
    output$view_var <- DT::renderDataTable(
      {
        if (is.null(input$file)) {
          return(NULL)
        } else {
          df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
          return(var_view(df))
        }
      },
      options = list(lengthChange = FALSE),
      rownames = F
    )

    output$view <- renderTable(NULL)
    output$view1 <- renderTable(NULL)
    output$view2 <- renderTable(NULL)
    output$boxPlot <- renderPlot(NULL, width = 500, height = 600)
  })


  observeEvent(input$act, {
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    g <- input$group
    me <- input$MEANS
    var <- input$variable
    var1 <- scan(var, ",")
    g1 <- scan(g, ",")
    me1 <- scan(me, ",")
    group1 <- df[, g1]
    var2 <- df[, var1]
    var2 <- as.matrix(var2, ncol = length(var1))
    xtitle <- c()
    for (i in 1:ncol(var2)) {
      xtitle[i] <- paste(colnames(df[var1])[i])
    }
    ytitle <- paste(colnames(df[g1]))

    #print(input$method) 20240507调试


    if (input$method == "one sample") {
      normaltest1 <- list()
      normaltest2 <- list()

      for (i in 1:ncol(var2)) {
        normaltest1[[i]] <- nor_test(var2[, i])
        normaltest2[[i]] <- lillie.test(var2[, i])$p.value
      }
      normaltest23 <- unlist(normaltest2)
      normaltest13 <- unlist(normaltest1)
      normaltest14 <- sprintf("%0.4f", normaltest13)
      normaltest24 <- sprintf("%0.4f", normaltest23)
      normaltest15 <- matrix(normaltest14, ncol = 1)
      normaltest25 <- matrix(normaltest24, ncol = 1)

      rownames <- colnames(df[var1])
      colnames <- c("Variable", "Shapiro-Wilk normality test (P)", "Kolmogorov-Smirnov normality test(P)")

      k1 <- cbind(rownames, normaltest15, normaltest25)
      colnames(k1) <- colnames

      jieguo_ttest1 <- list()
      jieguo_ttest2 <- list()
      jieguo_ttest3 <- list()
      jieguo_ttest11 <- list()
      jieguo_ttest12 <- list()
      jieguo_ttest21 <- list()
      jieguo_ttest31 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_ttest1[[i]] <- t.test(var2[, i], mu = me1)
        jieguo_ttest11[[i]] <- jieguo_ttest1[[i]]$p.value
        jieguo_ttest12[[i]] <- jieguo_ttest1[[i]]$statistic
        jieguo_ttest2[[i]] <- t.test(var2[, i], mu = me1, alternative = "less")
        jieguo_ttest21[[i]] <- jieguo_ttest2[[i]]$p.value
        jieguo_ttest3[[i]] <- t.test(var2[, i], mu = me1, alternative = "greater")
        jieguo_ttest31[[i]] <- jieguo_ttest3[[i]]$p.value
      }

      jieguo_ttest13 <- unlist(jieguo_ttest11)
      jieguo_ttest14 <- unlist(jieguo_ttest12)
      jieguo_ttest23 <- unlist(jieguo_ttest21)
      jieguo_ttest33 <- unlist(jieguo_ttest31)

      jieguo_ttest15 <- c()
      jieguo_ttest16 <- c()
      jieguo_ttest25 <- c()
      jieguo_ttest35 <- c()
      for (i in 1:ncol(var2)) {
        jieguo_ttest15[i] <- sprintf("%0.4f", jieguo_ttest13[i])
        jieguo_ttest16[i] <- sprintf("%0.4f", jieguo_ttest14[i])
        jieguo_ttest25[i] <- sprintf("%0.4f", jieguo_ttest23[i])
        jieguo_ttest35[i] <- sprintf("%0.4f", jieguo_ttest33[i])
      }

      rownames <- colnames(df[var1])

      f1 <- matrix(jieguo_ttest15, ncol = 1)
      f2 <- matrix(jieguo_ttest16, ncol = 1)
      f3 <- matrix(jieguo_ttest25, ncol = 1)
      f4 <- matrix(jieguo_ttest35, ncol = 1)

      jieguo_zhihe1 <- list()
      jieguo_zhihe2 <- list()
      jieguo_zhihe3 <- list()
      jieguo_zhihe11 <- list()
      jieguo_zhihe12 <- list()
      jieguo_zhihe21 <- list()
      jieguo_zhihe31 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_zhihe1[[i]] <- wilcox.test(var2[, i], mu = me1)
        jieguo_zhihe11[[i]] <- jieguo_zhihe1[[i]]$p.value
        jieguo_zhihe12[[i]] <- jieguo_zhihe1[[i]]$statistic
        jieguo_zhihe2[[i]] <- wilcox.test(var2[, i], mu = me1, alternative = "less")
        jieguo_zhihe21[[i]] <- jieguo_zhihe2[[i]]$p.value
        jieguo_zhihe3[[i]] <- wilcox.test(var2[, i], mu = me1, alternative = "greater")
        jieguo_zhihe31[[i]] <- jieguo_zhihe3[[i]]$p.value
      }
      jieguo_zhihe13 <- unlist(jieguo_zhihe11)
      jieguo_zhihe14 <- unlist(jieguo_zhihe12)
      jieguo_zhihe23 <- unlist(jieguo_zhihe21)
      jieguo_zhihe33 <- unlist(jieguo_zhihe31)

      jieguo_zhihe15 <- c()
      jieguo_zhihe16 <- c()
      jieguo_zhihe25 <- c()
      jieguo_zhihe35 <- c()
      for (i in 1:ncol(var2)) {
        jieguo_zhihe15[i] <- sprintf("%0.4f", jieguo_zhihe13[i])
        jieguo_zhihe16[i] <- sprintf("%0.4f", jieguo_zhihe14[i])
        jieguo_zhihe25[i] <- sprintf("%0.4f", jieguo_zhihe23[i])
        jieguo_zhihe35[i] <- sprintf("%0.4f", jieguo_zhihe33[i])
      }

      rownames <- colnames(df[var1])

      g1 <- matrix(jieguo_zhihe15, ncol = 1)
      g2 <- matrix(jieguo_zhihe16, ncol = 1)
      g3 <- matrix(jieguo_zhihe25, ncol = 1)
      g4 <- matrix(jieguo_zhihe35, ncol = 1)

      k2 <- cbind(rownames, f2, f1, g2, g1)
      colnames(k2) <- c("Variable", "t test (t)", "t test (P)", "Wilcoxon signed-rank test (T)", "Wilcoxon signed-rank test (P)")

      k3 <- cbind(rownames, f3, f4, g3, g4)
      colnames(k3) <- c("Variable", "t test (less)", "t test (greater)", "Wilcoxon signed-rank test (less)", "Wilcoxon signed-rank test (greater)")
    }



    if (input$method == "paired samples") {
      a <- list()
      normaltest1 <- list()
      normaltest2 <- list()
      for (i in 1:ncol(var2)) {
        d <- var2[, i][group1 == sort(unique(group1))[1]] - var2[, i][group1 == sort(unique(group1))[2]]
        normaltest1[[i]] <- nor_test(d)
        normaltest2[[i]] <- lillie.test(d)$p.value
      }

      normaltest23 <- unlist(normaltest2)
      normaltest13 <- unlist(normaltest1)
      normaltest14 <- sprintf("%0.4f", normaltest13)
      normaltest24 <- sprintf("%0.4f", normaltest23)
      normaltest15 <- matrix(normaltest14, ncol = 1)
      normaltest25 <- matrix(normaltest24, ncol = 1)

      rownames <- colnames(df[var1])
      colnames <- c("Variable", "Shapiro-Wilk normality test (P)", "Kolmogorov-Smirnov normality test (P)")

      k1 <- cbind(rownames, normaltest15, normaltest25)
      colnames(k1) <- colnames

      jieguo_ttest1 <- list()
      jieguo_ttest2 <- list()
      jieguo_ttest3 <- list()
      jieguo_ttest11 <- list()
      jieguo_ttest12 <- list()
      jieguo_ttest21 <- list()
      jieguo_ttest31 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_ttest1[[i]] <- t.test(var2[, i] ~ group1, paired = T)
        jieguo_ttest11[[i]] <- jieguo_ttest1[[i]]$p.value
        jieguo_ttest12[[i]] <- jieguo_ttest1[[i]]$statistic
        jieguo_ttest2[[i]] <- t.test(var2[, i] ~ group1, paired = T, alternative = "less")
        jieguo_ttest21[[i]] <- jieguo_ttest2[[i]]$p.value
        jieguo_ttest3[[i]] <- t.test(var2[, i] ~ group1, paired = T, alternative = "greater")
        jieguo_ttest31[[i]] <- jieguo_ttest3[[i]]$p.value
      }

      jieguo_ttest13 <- unlist(jieguo_ttest11)
      jieguo_ttest14 <- unlist(jieguo_ttest12)
      jieguo_ttest23 <- unlist(jieguo_ttest21)
      jieguo_ttest33 <- unlist(jieguo_ttest31)

      jieguo_ttest15 <- c()
      jieguo_ttest16 <- c()
      jieguo_ttest25 <- c()
      jieguo_ttest35 <- c()
      for (i in 1:ncol(var2)) {
        jieguo_ttest15[i] <- sprintf("%0.4f", jieguo_ttest13[i])
        jieguo_ttest16[i] <- sprintf("%0.4f", jieguo_ttest14[i])
        jieguo_ttest25[i] <- sprintf("%0.4f", jieguo_ttest23[i])
        jieguo_ttest35[i] <- sprintf("%0.4f", jieguo_ttest33[i])
      }

      rownames <- colnames(df[var1])

      f1 <- matrix(jieguo_ttest15, ncol = 1)
      f2 <- matrix(jieguo_ttest16, ncol = 1)
      f3 <- matrix(jieguo_ttest25, ncol = 1)
      f4 <- matrix(jieguo_ttest35, ncol = 1)


      jieguo_zhihe1 <- list()
      jieguo_zhihe2 <- list()
      jieguo_zhihe3 <- list()
      jieguo_zhihe11 <- list()
      jieguo_zhihe12 <- list()
      jieguo_zhihe21 <- list()
      jieguo_zhihe31 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_zhihe1[[i]] <- wilcox.test(var2[, i] ~ group1, paired = T)
        jieguo_zhihe11[[i]] <- jieguo_zhihe1[[i]]$p.value
        jieguo_zhihe12[[i]] <- jieguo_zhihe1[[i]]$statistic
        jieguo_zhihe2[[i]] <- wilcox.test(var2[, i] ~ group1, paired = T, alternative = "less")
        jieguo_zhihe21[[i]] <- jieguo_zhihe2[[i]]$p.value
        jieguo_zhihe3[[i]] <- wilcox.test(var2[, i] ~ group1, paired = T, alternative = "greater")
        jieguo_zhihe31[[i]] <- jieguo_zhihe3[[i]]$p.value
      }

      jieguo_zhihe13 <- unlist(jieguo_zhihe11)
      jieguo_zhihe14 <- unlist(jieguo_zhihe12)
      jieguo_zhihe23 <- unlist(jieguo_zhihe21)
      jieguo_zhihe33 <- unlist(jieguo_zhihe31)

      jieguo_zhihe15 <- c()
      jieguo_zhihe16 <- c()
      jieguo_zhihe25 <- c()
      jieguo_zhihe35 <- c()
      for (i in 1:ncol(var2)) {
        jieguo_zhihe15[i] <- sprintf("%0.4f", jieguo_zhihe13[i])
        jieguo_zhihe16[i] <- sprintf("%0.4f", jieguo_zhihe14[i])
        jieguo_zhihe25[i] <- sprintf("%0.4f", jieguo_zhihe23[i])
        jieguo_zhihe35[i] <- sprintf("%0.4f", jieguo_zhihe33[i])
      }

      rownames <- colnames(df[var1])

      g1 <- matrix(jieguo_zhihe15, ncol = 1)
      g2 <- matrix(jieguo_zhihe16, ncol = 1)
      g3 <- matrix(jieguo_zhihe25, ncol = 1)
      g4 <- matrix(jieguo_zhihe35, ncol = 1)

      k2 <- cbind(rownames, f2, f1, g2, g1)
      colnames(k2) <- c("Variable", "t test (t)", "t test (P)", "Wilcoxon signed-rank test (T)", "Wilcoxon signed-rank test (P)")

      k3 <- cbind(rownames, f3, f4, g3, g4)
      colnames(k3) <- c("Variable", "t test (less)", "t test (greater)", "Wilcoxon signed-rank test (less)", "Wilcoxon signed-rank test (greater)")
    }


    if (input$method == "two independent samples") {
      a <- list()
      normaltest1 <- list()
      normaltest2 <- list()
      for (i in 1:ncol(var2)) {
        normaltest1[[i]] <- aggregate(var2[, i], list(group1), nor_test)[, 2]
        normaltest2[[i]] <- aggregate(var2[, i], list(group1), lillie.test)[, 2][, 2]
      }

      normaltest11 <- unlist(normaltest1)
      normaltest21 <- unlist(normaltest2)
      normaltest12 <- sprintf("%0.4f", normaltest11)
      normaltest22 <- sprintf("%0.4f", normaltest21)
      normaltest13 <- matrix(normaltest12, ncol = length(unique(group1)), byrow = T)
      normaltest23 <- matrix(normaltest22, ncol = length(unique(group1)), byrow = T)

      fangcha <- list()
      fangcha1 <- list()
      for (i in 1:ncol(var2)) {
        fangcha[[i]] <- var.test(var2[, i] ~ as.factor(group1))
        fangcha1[[i]] <- fangcha[[i]]$p.value
      }

      fangcha2 <- unlist(fangcha1)
      fangcha3 <- sprintf("%0.4f", fangcha2)
      fangcha4 <- matrix(fangcha3, ncol = 1)
      g1_name <- paste0('(group=', sort(unique(group1))[1], ')')
      g2_name <- paste0('(group=', sort(unique(group1))[2], ')')

      rownames <- colnames(df[var1])
      colnames <- c(
        "Variable", paste0("Shapiro-Wilk normality test ", g1_name), paste0("Shapiro-Wilk normality test ", g2_name),
        paste0("Kolmogorov-Smirnov normality test ", g1_name), paste0("Kolmogorov-Smirnov normality test ", g2_name), "homogeneity of variance (F test)"
      )

      k1 <- cbind(rownames, normaltest13, normaltest23, fangcha4)
      colnames(k1) <- colnames

      jieguo_ttest1 <- list()
      jieguo_ttest2 <- list()
      jieguo_ttest3 <- list()
      jieguo_ttest11 <- list()
      jieguo_ttest12 <- list()
      jieguo_ttest21 <- list()
      jieguo_ttest31 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_ttest1[[i]] <- t.test(var2[, i] ~ group1, var.equal = T)
        jieguo_ttest11[[i]] <- jieguo_ttest1[[i]]$p.value
        jieguo_ttest12[[i]] <- jieguo_ttest1[[i]]$statistic
        jieguo_ttest2[[i]] <- t.test(var2[, i] ~ group1, var.equal = T, alternative = "less")
        jieguo_ttest21[[i]] <- jieguo_ttest2[[i]]$p.value
        jieguo_ttest3[[i]] <- t.test(var2[, i] ~ group1, var.equal = T, alternative = "greater")
        jieguo_ttest31[[i]] <- jieguo_ttest3[[i]]$p.value
      }

      jieguo_ttest13 <- unlist(jieguo_ttest11)
      jieguo_ttest14 <- unlist(jieguo_ttest12)
      jieguo_ttest23 <- unlist(jieguo_ttest21)
      jieguo_ttest33 <- unlist(jieguo_ttest31)

      jieguo_ttest15 <- c()
      jieguo_ttest16 <- c()
      jieguo_ttest25 <- c()
      jieguo_ttest35 <- c()
      for (i in 1:ncol(var2)) {
        jieguo_ttest15[i] <- sprintf("%0.4f", jieguo_ttest13[i])
        jieguo_ttest16[i] <- sprintf("%0.4f", jieguo_ttest14[i])
        jieguo_ttest25[i] <- sprintf("%0.4f", jieguo_ttest23[i])
        jieguo_ttest35[i] <- sprintf("%0.4f", jieguo_ttest33[i])
      }

      rownames <- colnames(df[var1])

      f1 <- matrix(jieguo_ttest15, ncol = 1)
      f2 <- matrix(jieguo_ttest16, ncol = 1)
      f3 <- matrix(jieguo_ttest25, ncol = 1)
      f4 <- matrix(jieguo_ttest35, ncol = 1)


      jieguo_Welch1 <- list()
      jieguo_Welch2 <- list()
      jieguo_Welch3 <- list()
      jieguo_Welch11 <- list()
      jieguo_Welch12 <- list()
      jieguo_Welch21 <- list()
      jieguo_Welch31 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_Welch1[[i]] <- t.test(var2[, i] ~ group1)
        jieguo_Welch11[[i]] <- jieguo_Welch1[[i]]$p.value
        jieguo_Welch12[[i]] <- jieguo_Welch1[[i]]$statistic
        jieguo_Welch2[[i]] <- t.test(var2[, i] ~ group1, alternative = "less")
        jieguo_Welch21[[i]] <- jieguo_Welch2[[i]]$p.value
        jieguo_Welch3[[i]] <- t.test(var2[, i] ~ group1, alternative = "greater")
        jieguo_Welch31[[i]] <- jieguo_Welch3[[i]]$p.value
      }

      jieguo_Welch13 <- unlist(jieguo_Welch11)
      jieguo_Welch14 <- unlist(jieguo_Welch12)
      jieguo_Welch23 <- unlist(jieguo_Welch21)
      jieguo_Welch33 <- unlist(jieguo_Welch31)

      jieguo_Welch15 <- c()
      jieguo_Welch16 <- c()
      jieguo_Welch25 <- c()
      jieguo_Welch35 <- c()
      for (i in 1:ncol(var2)) {
        jieguo_Welch15[i] <- sprintf("%0.4f", jieguo_Welch13[i])
        jieguo_Welch16[i] <- sprintf("%0.4f", jieguo_Welch14[i])
        jieguo_Welch25[i] <- sprintf("%0.4f", jieguo_Welch23[i])
        jieguo_Welch35[i] <- sprintf("%0.4f", jieguo_Welch33[i])
      }

      rownames <- colnames(df[var1])

      h1 <- matrix(jieguo_Welch15, ncol = 1)
      h2 <- matrix(jieguo_Welch16, ncol = 1)
      h3 <- matrix(jieguo_Welch25, ncol = 1)
      h4 <- matrix(jieguo_Welch35, ncol = 1)

      jieguo_zhihe1 <- list()
      jieguo_zhihe2 <- list()
      jieguo_zhihe3 <- list()
      jieguo_zhihe11 <- list()
      jieguo_zhihe12 <- list()
      jieguo_zhihe21 <- list()
      jieguo_zhihe31 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_zhihe1[[i]] <- wilcox.test(var2[, i] ~ group1, paired = F, alternative = "two.sided")
        jieguo_zhihe11[[i]] <- jieguo_zhihe1[[i]]$p.value
        f <- sum(rank(var2[,i])[group1==unique(group1)[1]])
        s <- sum(rank(var2[,i])[group1==unique(group1)[2]])
        jieguo_zhihe12[[i]] <- min(f,s)
        jieguo_zhihe2[[i]] <- wilcox.test(var2[, i] ~ group1, paired = F, alternative = "less")
        jieguo_zhihe21[[i]] <- jieguo_zhihe2[[i]]$p.value
        jieguo_zhihe3[[i]] <- wilcox.test(var2[, i] ~ group1, paired = F, alternative = "greater")
        jieguo_zhihe31[[i]] <- jieguo_zhihe3[[i]]$p.value
      }

      jieguo_zhihe13 <- unlist(jieguo_zhihe11)
      jieguo_zhihe14 <- unlist(jieguo_zhihe12)
      jieguo_zhihe23 <- unlist(jieguo_zhihe21)
      jieguo_zhihe33 <- unlist(jieguo_zhihe31)

      jieguo_zhihe15 <- c()
      jieguo_zhihe16 <- c()
      jieguo_zhihe25 <- c()
      jieguo_zhihe35 <- c()
      for (i in 1:ncol(var2)) {
        jieguo_zhihe15[i] <- sprintf("%0.4f", jieguo_zhihe13[i])
        jieguo_zhihe16[i] <- sprintf("%0.4f", jieguo_zhihe14[i])
        jieguo_zhihe25[i] <- sprintf("%0.4f", jieguo_zhihe23[i])
        jieguo_zhihe35[i] <- sprintf("%0.4f", jieguo_zhihe33[i])
      }

      rownames <- colnames(df[var1])

      g1 <- matrix(jieguo_zhihe15, ncol = 1)
      g2 <- matrix(jieguo_zhihe16, ncol = 1)
      g3 <- matrix(jieguo_zhihe25, ncol = 1)
      g4 <- matrix(jieguo_zhihe35, ncol = 1)

      k2 <- cbind(rownames, f2, f1, h2, h1, g2, g1)
      colnames(k2) <- c("Variable", "t test (t)", "t test (P)", "Welch's t test (t)", "Welch's t test (P)", "Wilcoxon rank sum test (T)", "Wilcoxon rank sum test (P)")

      k3 <- cbind(rownames, f3, f4, h3, h4, g3, g4)
      colnames(k3) <- c("Variable", "t test (less)", "t test (greater)", "Welch's t test (less)", "Welch's t test (greater)", "Wilcoxon rank sum test (less)", "Wilcoxon rank sum test (greater)")
    }


    output$title <- renderText(c("Normality and homogeneity of variance (P values)"))
    output$view_var <- DT::renderDataTable(NULL, options = list(lengthChange = FALSE))


    output$view <- renderTable(head(k1, n = 10), rownames = F)
    output$view1 <- renderTable(head(k2, n = 10), rownames = F)
    output$view2 <- renderTable(head(k3, n = 10), rownames = F)


    output$boxPlot <- renderPlot(
      {
        if (is.null(input$file)) {
          return(NULL)
        }
        df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
        if (input$variable == "") {
          plot <- "Please input the correct column number"
        }

        if (input$variable != "") {
          par(cex.axis = 1.6, cex.lab = 2)
          par(mai = rep(1, 4))
          par(no.readonly = TRUE)
          par(mfrow = c(4, 2))
          if (input$method == "one sample") {
            for (i in 1:ncol(var2)) {
              plot <- boxplot(var2[, i], notch = TRUE, col = c("green"), ylab = xtitle[i])
            }
          }

          if (input$method == "paired samples") {
            for (i in 1:ncol(var2)) {
              plot <- boxplot(var2[, i] ~ group1, notch = TRUE, col = c("green"), xlab = ytitle, ylab = xtitle[i])
            }
          }

          if (input$method == "two independent samples") {
            for (i in 1:ncol(var2)) {
              plot <- boxplot(var2[, i] ~ group1, notch = TRUE, col = c("green"), xlab = ytitle, ylab = xtitle[i])
            }
          }
        }
      },
      width = 800,
      height = 1600
    )

    output$downloadTable <- downloadHandler(
      filename = function() {
        "ttest.xlsx"
      },
      content = function(file) {
        re_tab <- list("ttest" = k2, "ttest (one-sided)" = k3)
        openxlsx::write.xlsx(re_tab, file = file, rowNames = T)
      }
    )

    output$downloadPlot <- downloadHandler(
      filename = function() {
        "boxplot.pdf"
      },
      content = function(file) {
        pdf(file, onefile = TRUE)
        if (input$variable == "") {
          plot <- "Please input the correct column number"
        }
        if (input$variable != "") {
          if (input$method == "one sample") {
            par(no.readonly = TRUE)
            for (i in 1:ncol(var2)) {
              plot <- boxplot(var2[, i], notch = TRUE, col = c("green"), ylab = xtitle[i])
            }
          }

          if (input$method == "paired samples") {
            par(no.readonly = TRUE)
            for (i in 1:ncol(var2)) {
              plot <- boxplot(var2[, i] ~ group1, notch = TRUE, col = c("green"), ylab = xtitle[i], xlab = ytitle)
            }
          }

          if (input$method == "two independent samples") {
            par(no.readonly = TRUE)
            for (i in 1:ncol(var2)) {
              plot <- boxplot(var2[, i] ~ group1, notch = TRUE, col = c("green"), ylab = xtitle[i], xlab = ytitle)
            }
          }
        }

        dev.off()
      }
    )
  })
}
