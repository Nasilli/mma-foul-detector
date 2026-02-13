# if you haven't already
library(reticulate)

# Install a private Miniconda Python for R, one-time only:
reticulate::install_miniconda()

conda_create("mma-nlp", packages = "python=3.9")

# tell reticulate to use it
use_condaenv("mma-nlp", required = TRUE)

py_run_string("import pip")
py_run_string("pip.main(['install', 'allennlp==2.10.1'])")
py_run_string("pip.main(['install', 'allennlp-models==2.10.1'])")
py_run_string("pip.main(['install', 'transformers'])")



use_condaenv("mma-nlp", required = TRUE)

py_run_string("import allennlp; print('AllenNLP loaded OK')")
py_run_string("import transformers; print('Transformers loaded OK')")