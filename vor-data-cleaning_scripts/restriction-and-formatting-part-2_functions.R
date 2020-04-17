# restriction-and-formatting-part-2_functions

# -------------
# utility functions
# -------------
    # complete case (applied in discrete time step)

    # exclude early years (e.g., do not have OP_NPI for 2010)




# -------------
# combined/compiling function
# -------------
combo.pack.2_function <- function(dat, ...){
  dat %>% restrict.after.year_function(dat=., ...) %>% 
    complete.case_function(dat=.)
}
