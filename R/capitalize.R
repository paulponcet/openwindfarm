
capitalize <- 
function(x)
{
  gsub("(^[[:alpha:]])", "\\U\\1", x, perl = TRUE)
}
