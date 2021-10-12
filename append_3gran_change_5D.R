# read_rds("js-nqt/3gran_Change_5D/confmatrix_27.rds") %>% View
# 
# simtable %>% dplyr::filter(mean_diff==1)


folder_name = "js_robust"



append_files <- function(folder_name){
all_files = list.files(path = paste0(folder_name, "/3gran_change_5D/"), 
                       pattern = ".rds")

names_levels <- map_dfr(all_files, 
                        function(x){
                          z = str_split(str_remove(x, ".rds"), "_") %>% 
                            unlist()
                          bind_cols(type = z[1],
                                    index = z[2])
                        })


all_files_path <- paste0(folder_name, "/3gran_Change_5D/",
                         all_files)  


all_data <- lapply(1:length(all_files_path), function(x){
  
  data = all_files_path %>% magrittr::extract2(x) %>% 
    readRDS()
  
  names = names_levels %>% magrittr::extract(x,)
  names_rep =   names %>% slice(rep(1:n(), each = nrow(data)))
  bind_cols(names_rep, data)
}) %>% bind_rows() 
}

wpd <- append_files("wpd") %>% 
  dplyr::filter(type == "confmatrix", 
                term == "accuracy") %>% 
  arrange(mean_diff, nT, niter) %>% 
  dplyr::select (mean_diff, nT, niter, estimate) %>% mutate(method = "wpd")


js_robust <- append_files("js-robust") %>% 
  dplyr::filter(type == "confmatrix", 
                term == "accuracy") %>% 
  arrange(mean_diff, nT, niter) %>% 
  dplyr::select (mean_diff, nT, niter, estimate) %>% mutate(method = "js_robust")


js_nqt <- append_files("js-nqt") %>% 
  dplyr::filter(type == "confmatrix", 
                term == "accuracy") %>% 
  arrange(mean_diff, nT, niter) %>% 
  dplyr::select (mean_diff, nT, niter, estimate) %>% mutate(method = "js_nqt")



data_all<- bind_rows(wpd, js_robust, js_nqt) %>% 
  #arrange(method, nT, niter, mean_diff) %>% 
  pivot_wider(c(1:3), names_from = "method", values_from = "estimate")


write_rds(data_all, "data/append_3gran_change.rds")
