; library code

define i32 @main(i32, i8**) {
    %main.ret.ptr = alloca i32
    call void @.main(i32* %main.ret.ptr)
    %main.ret = load i32, i32* %main.ret.ptr
    ret i32 %main.ret
}

@print_str.ptr = private constant [4 x i8] c"%s\0A\00"
@print_int.ptr = private constant [4 x i8] c"%d\0A\00"
@print_flt.ptr = private constant [4 x i8] c"%f\0A\00"
@input_buffer_len.ptr = private constant i32 128

define void @.print_str (i32* %ret.value, i8* %str) {
    %print_str.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_str.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_str.ptr, i8* %str)
    store i32 %ret, i32* %ret.value
    ret void 
}

define void @.print_int (i32* %ret.value, i32 %i) {
    %print_int.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_int.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_int.ptr, i32 %i)
    store i32 %ret, i32* %ret.value
    ret void 
}

define void @.print_flt (i32* %ret.value, double %d) {
    %print_flt.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_flt.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_flt.ptr, double %d)
    store i32 %ret, i32* %ret.value
    ret void 
}

define void @.print_bln (i32* %ret.value, i1 %b) {
    %print_bln.ptr = getelementptr inbounds [4 x i8], [4 x i8]* @print_int.ptr, i32 0, i32 0
    %ret = call i32 (i8*, ...) @printf(i8* %print_bln.ptr, i1 %b)
    store i32 %ret, i32* %ret.value
    ret void 
}

define void @.str_to_int (i32* %ret.value, i8* %str) {
    %intval = call i32 @atoi(i8* %str, i8** null)
    store i32 %intval, i32* %ret.value
    ret void
}

define void @.str_to_flt (double* %ret.value, i8* %str) {
    %fval = call double @strtod(i8* %str, i8** null)
    store double %fval, double* %ret.value
    ret void
}

define void @.input (i8** %ret.value1) {
    %len_max = load i32, i32* @input_buffer_len.ptr
    %len_max64 = zext i32 %len_max to i64

    %current_size.ptr = alloca i32
    store i32 %len_max, i32* %current_size.ptr

    %ch.ptr = alloca i32
    store i32 -1, i32* %ch.ptr

    %i.ptr = alloca i32
    store i32 0, i32* %i.ptr

    %buffer.ptr = alloca i8*	
    %buffer = call i8* @malloc(i64 %len_max64)
    store i8* %buffer, i8** %buffer.ptr

    br label %label.loop.begin1

    label.loop.begin1:
    %.3.ptr = alloca i1
    %ch = call i32 @getchar()
    store i32 %ch, i32* %ch.ptr

    %.4 = icmp eq i32 %ch, 10
    %.5 = xor i1 %.4, 1
    store i1 %.5, i1* %.3.ptr
    br i1 %.5, label %label.and.left1, label %label.and.right1

    label.and.left1:
    %ch1 = load i32, i32* %ch.ptr
    %.8 = icmp eq i32 %ch1, -1
    %.9 = xor i1 %.8, 1
    store i1 %.9, i1* %.3.ptr
    br label %label.and.right1

    label.and.right1:
    %.3 = load i1, i1* %.3.ptr
    br i1 %.3, label %label.loop.then1, label %label.loop.end1

    label.loop.then1:
    %i.prev = load i32, i32* %i.ptr
    %i.next = add i32 %i.prev, 1

    %ch2 = load i32, i32* %ch.ptr
    %ch.val = trunc i32 %ch2 to i8
    %buff.cur = load i8*, i8** %buffer.ptr
    %buff.ptr = getelementptr inbounds i8, i8* %buff.cur, i32 %i.prev

    store i8 %ch.val, i8* %buff.ptr
    store i32 %i.next, i32* %i.ptr	

    %.11 = load i32, i32* %current_size.ptr
    %.12 = icmp eq i32 %i.next, %.11
    br i1 %.12, label %label.cond.then1, label %label.cond.fi1

    label.cond.then1:
    %.13 = load i32, i32* %i.ptr
    %size.next = add i32 %.13, %len_max
    store i32 %size.next, i32* %current_size.ptr
    %size64 = zext i32 %size.next to i64

    %buff.cur1 = load i8*, i8** %buffer.ptr
    %buff.next = call i8* @realloc(i8* %buff.cur1, i64 %size64)	
    store i8* %buff.next, i8** %buffer.ptr
    br label %label.cond.fi1

    label.cond.fi1:
    br label %label.loop.begin1

    label.loop.end1:

    %buff.last = load i8*, i8** %buffer.ptr
    %i.last = load i32, i32* %i.ptr
    %buff.last.end = getelementptr inbounds i8, i8* %buff.last, i32 %i.last
    store i8 0, i8* %buff.last.end
    store i8* %buff.last, i8** %ret.value1
    br label %label.ret

    label.ret:
    ret void
}

declare double @strtod(i8*, i8**)
declare i32 @atoi(i8*, i8**)
declare i8* @malloc(i64)
declare i32 @getchar()
declare i8* @realloc(i8*, i64)
declare i32 @printf(i8*, ...)

