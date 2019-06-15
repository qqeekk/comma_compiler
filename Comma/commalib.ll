; library code
@print_str.ptr = private constant [4 x i8] c"%s\0A\00"
@print_int.ptr = private constant [4 x i8] c"%d\0A\00"
@print_flt.ptr = private constant [4 x i8] c"%f\0A\00"

define i32 @main(i32, i8**) {
    %main.ret.ptr = alloca i32
    call void @.main(i32* %main.ret.ptr)
    %main.ret = load i32, i32* %main.ret.ptr
    ret i32 %main.ret
}

define void @print_str (i32* %ret.value, i8* %str) {
    %print_str.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_str.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_str.ptr, i8* %str)
    store i32 %ret, i32* %ret.value
    ret void 
}

define void @print_int (i32* %ret.value, i32 %i) {
    %print_int.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_int.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_int.ptr, i32 %i)
    store i32 %ret, i32* %ret.value
    ret void 
}

define void @print_flt (i32* %ret.value, double %d) {
    %print_flt.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_flt.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_flt.ptr, double %d)
    store i32 %ret, i32* %ret.value
    ret void 
}

define void @print_bln (i32* %ret.value, i1 %b) {
    %print_bln.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_int.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_bln.ptr, i1 %b)
    store i32 %ret, i32* %ret.value
    ret void 
}

declare i32 @printf(i8*, ...)
