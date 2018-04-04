#' tsne
#' 
#' @param X matrix; Data matrix
#' @param dims	integer; Output dimensionality (default: 2)
#' @param initial_dims integer; the number of dimensions that should be retained in the initial PCA step (default: -1 (all))
#' @param perplexity numeric; Perplexity parameter
#' @param theta	numeric; Speed/accuracy trade-off (increase for less accuracy, default: 0.5)
#' @param pca	logical; Whether an initial PCA step should be performed (default: FALSE)
#' @param max_iter	integer; Number of iterations (default: 1000)
#' @param verbose logical; Whether progress updates should be printed (default: FALSE)
#' 
#' @importFrom rJava .jnew .jcall
#' @export
tsne <- function(X, dims=2, initial_dims=-1, perplexity=30, max_iter=100,
                   pca=FALSE, theta=0.6, verbose=FALSE) {
  print_error <- verbose
  # Using Boolean instead of boolean does not work
  #up <- .jnew("java.lang.Boolean",use_pca)
  #pe <- .jnew("java.lang.Boolean",print_error)
  #sl <- .jnew("java.lang.Boolean",silent)
  # X MUST be a double matrix
  conf <- .jnew("com.jujutsu.tsne.TSneConfig",
                .jarray(X,dispatch = T),
                as.integer(dims),
                as.integer(initial_dims),
                as.numeric(perplexity),
                as.integer(max_iter),
                pca,
                as.numeric(theta),
                verbose,
                print_error)
  #TSneConfig(double[][] X, int outputDims, int initial_dims, double perplexity, int max_iter,
  # boolean use_pca, double theta, boolean silent, boolean print_error)
  # Below works but is unnecessary
  # conf <- .jnew("com.jujutsu.tsne.TSneConfig")
  # .jcall(conf,"V","setXin",.jarray(X,dispatch = T))
  # .jcall(conf,"V","setOutputDims",as.integer(output_dims))
  # .jcall(conf,"V","setInitialDims",as.integer(initial_dims))
  # .jcall(conf,"V","setPerplexity",perplexity)
  # .jcall(conf,"V","setMaxIter",as.integer(max_iter))
  # .jcall(conf,"V","setUsePca",use_pca)
  # .jcall(conf,"V","setTheta",theta)
  # .jcall(conf,"V","setSilent",silent)
  # .jcall(conf,"V","setPrintError",print_error)
  
  cfg <- .jcast(conf,"com.jujutsu.tsne.TSneConfiguration")
  
  util <- .jnew("com.jujutsu.utils.TSneUtils")
  cr <- .jcall(util,"Lcom/jujutsu/tsne/CheckResult;","check",cfg)
  if(.jcall(cr,"Z","check")) {
    tsne <- .jnew("com.jujutsu.tsne.barneshut.ParallelBHTsne")
    ds <- .jcall(tsne,"[[D","tsne",cfg,evalArray = TRUE,simplify = TRUE)
    return(ds)
  } else {
    stop(.jcall(cr,"S","getExplanation"))
  }

}
