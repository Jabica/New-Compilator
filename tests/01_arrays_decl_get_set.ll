; ModuleID = 'mycc_module'
source_filename = "mycc_module"

declare void @printi(i32)

declare void @printb(i1)

define i32 @main() {
entry:
  %s = alloca i32, align 4
  %v = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 4, ptr %n, align 4
  %v.elem.ptr = getelementptr inbounds i32, ptr %v, i32 0
  store i32 10, ptr %v.elem.ptr, align 4
  %v.elem.ptr1 = getelementptr inbounds i32, ptr %v, i32 1
  store i32 11, ptr %v.elem.ptr1, align 4
  %v.elem.ptr2 = getelementptr inbounds i32, ptr %v, i32 2
  store i32 12, ptr %v.elem.ptr2, align 4
  %v.elem.ptr3 = getelementptr inbounds i32, ptr %v, i32 3
  store i32 13, ptr %v.elem.ptr3, align 4
  %v.elem.ptr4 = getelementptr inbounds i32, ptr %v, i32 0
  %v.elem = load i32, ptr %v.elem.ptr4, align 4
  %v.elem.ptr5 = getelementptr inbounds i32, ptr %v, i32 1
  %v.elem6 = load i32, ptr %v.elem.ptr5, align 4
  %addtmp = add i32 %v.elem, %v.elem6
  %v.elem.ptr7 = getelementptr inbounds i32, ptr %v, i32 2
  %v.elem8 = load i32, ptr %v.elem.ptr7, align 4
  %addtmp9 = add i32 %addtmp, %v.elem8
  %v.elem.ptr10 = getelementptr inbounds i32, ptr %v, i32 3
  %v.elem11 = load i32, ptr %v.elem.ptr10, align 4
  %addtmp12 = add i32 %addtmp9, %v.elem11
  store i32 %addtmp12, ptr %s, align 4
  %s.val = load i32, ptr %s, align 4
  ret i32 %s.val
}
