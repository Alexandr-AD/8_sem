start:

.include "m16def.inc" ;подключение библиотеки для работы с ATmega16
.list ; включение листинга 
.def temp=r16 ; определение главного рабочего регистра
.def kold=r17 
.def k=r18 
.def s=r19 
;-------------------------------------------- 
.cseg  ; выбор сегмента программного кода
.org 0 ; установка текущего адреса на ноль
;-------------------------------------------- 
ldi temp,0x80 ;  выключение компаратора
out acsr,temp 
;-------------------------------------------- 
ldi temp,0x00 ; 0 --> temp 
out ddrd,temp ; Назначаем порт rd на ввод (00000000 --> ddrd)
ldi temp,0xFF ; 0xff --> temp 
out ddrb,temp ; Назначаем порт rb на вывод (11111111 --> ddrb)
out portd,temp ;  Подключаем подтягивающие резисторы (11111111 --> portd)
;--------------------------------------------- 
ldi  kold, 0x00 ; 0--->kold 
ldi  s, 0x00 ; 0--->s___ 
out  portb, s ;Вывели s___(=0)
;--------------------------------------------- 
read:   ; 
in  k, pind ;  Считали содержимое порта pd (--->k___)
cp  k, kold ; Сравнили k___ и kold
breq read  ;  Если k___=kold, read
tst  kold  ; Проверили kold
brne remem  ;  Если kold!=0, remen
tst  k  ; Проверили k___
breq remem  ; Если k___=0, remem
jmp  lbl1  ; 
;--------------------------------------------- 
remem:   ; 
mov kold, k ; Записали k___ в kold 
jmp  read  ; Вернулись в read
;--------------------------------------------- 

inc_x3: ;увеличение числа на 3
inc s
inc s
inc s
jmp print

dec_x3: ;уменьшение числа на 3
dec s
dec s
dec s
jmp print

inc_x5: ;увеличение числа на 5
inc s
inc s
inc s
inc s
inc s
jmp print

dec_x5: ;уменьшение числа на 5
dec s
dec s
dec s
dec s
dec s
jmp print


lbl1: 
cpi k, 0x01 ; Сравнили k___ и 1
breq black

cpi k,0x02 ; Сравнили k___ и 2
breq red

cpi k,0x04 ; Сравнили k___ и 4
breq blue

cpi k,0x08 ; Сравнили k___ и 8
breq sbros

jmp read ; Вернулись в read

sbros:
ldi  s, 0x00 
jmp print

red:
cpi s,0x01 ;проверяем число
breq inc_x3 ;выполняем необходимую по алгоритму операцию
cpi s,0x0B
breq inc_x3
cpi s,0x02
breq inc_x5
cpi s,0x08
breq inc_x5
cpi s,0x04
breq dec_x3
cpi s,0x0E
breq dec_x3
cpi s,0x07
breq dec_x5
cpi s,0x0D
breq dec_x5
jmp read

black:
cpi s,0x04
breq inc_x1
cpi s,0x06
breq inc_x1
cpi s,0x05
breq dec_x4
cpi s,0x0D
breq dec_x4
cpi s,0x02
breq inc_x4
cpi s,0x0A
breq inc_x4
cpi s,0x09
breq dec_x1
cpi s,0x0B
breq dec_x1
jmp read

blue:
cpi s,0x00
breq inc_x1
cpi s,0x01
breq inc_x1
cpi s,0x02
breq inc_x1
cpi s,0x05
breq inc_x1
cpi s,0x0D
breq dec_x1
cpi s,0x0E
breq dec_x1
cpi s,0x0F
breq dec_x1
cpi s,0x0A
breq dec_x1
cpi s,0x04
breq dec_x4 
cpi s,0x08
breq dec_x4 
cpi s,0x0C
breq dec_x4 
cpi s,0x09
breq dec_x4 
cpi s,0x03
breq inc_x4  
cpi s,0x06
breq inc_x4   
cpi s,0x07
breq inc_x4  
cpi s,0x0B
breq inc_x4    
jmp read
;-------------
inc_x4: ;увеличение числа на 4
inc s
inc s
inc s
inc s
jmp print

inc_x1: ;увеличение числа на 1
inc s
jmp print

dec_x1: ;уменьшение числа на 1
dec s
jmp print

dec_x4: ;уменьшение числа на 4
dec s
dec s
dec s
dec s
jmp print

print:   ; 
out  portb, s ; Вывели s___ в порт pb
jmp  remem