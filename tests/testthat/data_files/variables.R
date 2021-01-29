vector_A <- c(1:71)
vector_B <- vector_A * 100000
vector_C <- vector_A + 1

matrix_A <- matrix(c(1:100), 10, 10)
matrix_B <- matrix(c(1000:1099), 5, 20)
matrix_C <- matrix(c(1:100), 10, 10)

list_A   <- list(vector_A)
list_B   <- list(vector_B)
list_C   <- list(vector_C)

df_A     <- data.frame(vector_A,vector_B, list_C)
df_B     <- data.frame(vector_A * -10000000 , vector_B * -10000000, vector_C * -10000000)
df_C     <- data.frame(vector_A + 1, vector_B + 1, vector_C + 1)

assign("vector_A", vector_A, pos = 1)
assign("vector_B", vector_B, pos = 1)
assign("vector_C", vector_C, pos = 1)

assign("matrix_A", matrix_A, pos = 1)
assign("matrix_B", matrix_B, pos = 1)
assign("matrix_C", matrix_C, pos = 1)

assign("list_A", list_A, pos = 1)
assign("list_B", list_B, pos = 1)
assign("list_C", list_C, pos = 1)

assign("df_A", df_A, pos = 1)
assign("df_B", df_B, pos = 1)
assign("df_C", df_C, pos = 1)

assign("D", read.csv("data_files/DATASET1.csv", header = TRUE), pos = 1)
assign("E", read.csv("data_files/DATASET2.csv", header = TRUE), pos = 1)
assign("F", read.csv("data_files/DATASET3.csv", header = TRUE), pos = 1)


assign("all.data", rbind(get("D", pos = 1), get("E", pos = 1), get("F", pos = 1)) , pos = 1)





assign("PI", pi, pos = 1)



vector_a <- get("vector_A", pos = 1)
vector_b <- get("vector_B", pos = 1)
vector_c <- get("vector_C", pos = 1)

vector_a_copy <- vector_a - 0.00001
vec_a_char    <- as.character(vector_A)
vec_b_char    <- as.character(vector_B)

matrix_a <- get("matrix_A", pos = 1)
matrix_b <- get("matrix_B", pos = 1)
matrix_c <- get("matrix_C", pos = 1)

list_a <- get("list_A", pos = 1)
list_b <- get("list_B", pos = 1)
list_c <- get("list_C", pos = 1)

df_a <- get("df_A", pos = 1)
df_b <- get("df_B", pos = 1)
df_c <- get("df_C", pos = 1)
