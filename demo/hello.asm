.0

mov p %STR
out p %4
hlt

; Data/var section
db STR "Hello world\n\0"
dd p 0
