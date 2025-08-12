library(forcats) # For fct_rev
library(ANTsR)
library(ggrepel)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(viridis)
library(ggforce) # For geom_mark_ellipse

set.seed(808)
     nsub <- 250
     npix <- c(100, 200, 133)
     nk <- 8
     outcome <- matrix(rnorm(nsub * nk), ncol = nk)
     outcome1 <- matrix(rnorm(nsub * nk), ncol = nk)
     outcome2 <- matrix(rnorm(nsub * nk), ncol = nk)
     outcome3 <- matrix(rnorm(nsub * nk), ncol = nk)
     view1tx <- matrix(rnorm(npix[1] * nk), nrow = nk)
     view2tx <- matrix(rnorm(npix[2] * nk), nrow = nk)
     view3tx <- matrix(rnorm(npix[3] * nk), nrow = nk)
     mat1 <- (outcome %*% t(outcome1) %*% (outcome1)) %*% view1tx
     mat2 <- (outcome %*% t(outcome2) %*% (outcome2)) %*% view2tx
     mat3 <- (outcome %*% t(outcome3) %*% (outcome3)) %*% view3tx
     voxmats <- list(vox = mat1, vox2 = mat2, vox3 = mat3)
#    result <- simlr(voxmats)

# --- New Function to Extract Ortho Strength ---
extract_ortho_strength <- function(constraint_str) {
  if (constraint_str == 'none') {
    return(0.0)
  } else if (grepl("^orthox", constraint_str)) {
    parts <- strsplit(constraint_str, "x")[[1]]
    if (length(parts) >= 2 && parts[1] == "ortho") {
      strength_str <- parts[2]
      # Handle the 'x1' suffix in the denominator if it exists
      if (grepl("x1$", strength_str)) {
        strength_str <- sub("x1$", "", strength_str)
      }
      
      strength_val <- as.numeric(strength_str)
      
      # Check if conversion was successful and value is reasonable
      if (!is.na(strength_val) && strength_val >= 0 && strength_val <= 1) {
        return(strength_val)
      }
    }
  }
  # Default or error case
  return(NA_real_) 
}


orthos=c("orthox1x2",  "orthox0.12x2", "orthox0.08x2", "orthox0.04x2", "orthox0.01x2","orthox0.005x2","orthox0.002x2","orthox0.001x2", "orthox0x0" )
if( ! exists("mydf")) {
mydf=data.frame()
for ( m in c("ica","pca","svd") )
for ( e in c("regression","nc","acc","lrr"))
for ( o in orthos ) {
  result <- simlr(voxmats,verbose=0,energyType=e,mixAlg=m,iterations=500,
    constraint=o, randomSeed=808, optimizationStyle='ls_adam')
  n=nrow(mydf)+1
  mydf[n,'omega']=extract_ortho_strength(o)
  mydf[n,'defect']=invariant_orthogonality_defect(result$v[[1]])
  mydf[n,'energy']=result$finalError
  mydf[n,'objective']=paste0(e,'.',m)
  print(mydf[n,])
}
df=mydf
}

print( mydf )

plot1 <- ggplot(df, aes(x = omega, y = energy)) +
  geom_point(aes(color = objective), alpha = 0.7) + # Points with color for each objective
  geom_line(aes(color = objective), alpha = 0.5) +   # Lines to connect points within each objective
  facet_wrap(~ objective, scales = "free") +       # Facet by objective, allowing scales to adjust
  labs(
    title = "Energy vs. Omega across Different Objectives",
    subtitle = "Facetted by Objective Type",
    x = "Omega Value",
    y = "Energy Value",
    color = "Objective"
  ) +
  theme_minimal() + # A clean, minimal theme
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12) # Bold facet titles
  )

print("Generating Plot 1: Energy vs. Omega (Faceted by Objective)")
print(plot1)



plot2 <- ggplot(df, aes(x = omega, y = defect)) +
  geom_point(aes(color = objective), alpha = 0.7) +
  geom_line(aes(color = objective), alpha = 0.5) +
  facet_wrap(~ objective, scales = "free") +
  labs(
    title = "Defect vs. Omega across Different Objectives",
    subtitle = "Facetted by Objective Type",
    x = "Omega Value",
    y = "Defect Value",
    color = "Objective"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12)
  )

print("Generating Plot 2: Defect vs. Omega (Faceted by Objective)")
print(plot2)


plot3 <- ggplot(df, aes(x = defect, y = energy)) +
  geom_point(aes(color = objective), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, aes(color = objective), alpha = 0.5) + # Add linear model for trend
  facet_wrap(~ objective, scales = "free") +
  labs(
    title = "Energy vs. Defect across Different Objectives",
    subtitle = "Facetted by Objective Type, with Linear Trend Lines",
    x = "Defect Value",
    y = "Energy Value",
    color = "Objective"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    strip.text = element_text(face = "bold", size = 12)
  )

print("Generating Plot 3: Energy vs. Defect (Faceted by Objective)")
print(plot3)


plot4_violin <- ggplot(df, aes(x = objective, y = omega, fill = objective)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) + # Add jittered points for more detail
  labs(
    title = "Distribution of Omega by Objective",
    subtitle = "Violin Plot Showing Density and Individual Data Points",
    x = "Objective",
    y = "Omega Value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Hide legend as colors are redundant with x-axis
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(size = 10)
  )

print("Generating Plot 4: Distribution of Omega by Objective (Violin Plot)")
print(plot4_violin)
