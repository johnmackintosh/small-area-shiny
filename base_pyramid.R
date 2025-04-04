base_pyramid <- function(areaname,
                         axis_by = NULL,
                         male_value = "Males",
                         female_value =  "Females",
                         male_colour = "#0072B2",
                         female_colour = "#56B4E9",
                         plot_title = NULL,
                         gridlines){

  cps <- areaname

  maximals <- read.csv("./maximal_values.txt")
  max_value <- maximals[maximals$CP_Name == cps,2]

  # derive axis_by if not provided
  if(is.null(axis_by)){

    if(max_value >= 3000){
      axis_by <- 1000
    }

    if(max_value < 3000){
      axis_by <- 500
    }


  if(max_value <= 1000){
    axis_by <- 100
  }
  }


  roundup <- function(from,to) ceiling(from/to)*to
  max_value <- roundup(max_value, axis_by)
  low_val <- max_value * -1


  # create plot_tile if not provided
  if (is.null(plot_title)) {

    plot_title <- paste0("Population of ", cps, " by five year age band")
  }


  t1 <- read.csv("cp-age-sex-pops.csv")
  t1 <- t1[t1$CP_Name == cps,]


  t1 <- within(t1, pop <- ifelse(sex == male_value, pop * -1, pop))

  females <- t1[t1$sex == female_value,]
  males <- t1[t1$sex == male_value,]



  barplot(height = males$pop,
          col = male_colour,
          border = NA,
          horiz = TRUE,
          axes = FALSE,
          names.arg = c("0-4", "5-9", "10-14", "15-19", "20-24" ,
                        "25-29", "30-34", "35-39", "40-44", "45-49",
                        "50-54", "55-59", "60-64", "65-69", "70-74",
                        "75-79", "80-84", "85-89", "90+"),
          axisnames = TRUE,
          xlim = c(low_val, max_value),
          las = 1,
          tcl = 0,
          xlab = "Population")

  if (gridlines) {

    grid()

    barplot(height = males$pop,
            col = male_colour,
            border = NA,
            horiz = TRUE,
            axes = FALSE,
            names.arg = c("0-4", "5-9", "10-14", "15-19", "20-24" ,
                          "25-29", "30-34", "35-39", "40-44", "45-49",
                          "50-54", "55-59", "60-64", "65-69", "70-74",
                          "75-79", "80-84", "85-89", "90+"),
            axisnames = TRUE,
            xlim = c(low_val, max_value),
            las = 1,
            tcl = 0,
            xlab = "Population",
            add = TRUE)

  }



  barplot(height = females$pop,
          col = female_colour,
          horiz = TRUE,
          axes = FALSE,
          border = NA,
          add = TRUE,
          las = 1,
          tcl = 0)

  ## axes
  axis(1,
       at = seq(low_val,max_value, by = axis_by),
       labels = abs(seq(low_val, max_value, by = axis_by)),
       tck = 0,
       lwd = 0)

  title(main = plot_title,
         adj = 0) # plot title left aligned)

  # add subtitle
  mtext(side = 3,
        adj = 0,
        cex = 0.8,
        text = 'Contains public sector information licensed under the Open Government Licence v3.0')

  legend("topright",
         legend = c(male_value, female_value),
         fill = c(male_colour, female_colour),
         box.lty = 0,
         cex = 0.8)



}
