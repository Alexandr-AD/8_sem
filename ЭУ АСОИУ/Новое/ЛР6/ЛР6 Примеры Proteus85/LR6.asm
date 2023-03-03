.include "m16def.inc" ; подключение библиотеки для работы с ATmega16
.list ; включение листинга
.def temp=r16 ; определение главного рабочего регистра
.def reg_cmp=r17 
.def o___=r18 
.def a___=r19 
.def res___=r20
.def t___=r21
.def k___=r22
.cseg ; выбор сегмента программного кода
.org 0 ; установка текущего адреса на ноль

;-----------------------

ldi temp,0x80 ; выключение компаратора
out acsr,temp 

;-----------------------

ldi temp,0x00 ; 0 --> temp
out ddrd,temp ; Назначаем порт rd на ввод (00000000 --> ddrd)
ldi temp,0xFF ; 0xff --> temp
out ddra,temp 
out ddrc,temp t

;-----------------------

ldi temp,0b110 ; Предделение 1024
out tccr0,temp
ldi temp,low(RAMEND)  ; инициализация стека
out spl,temp
ldi temp,high(RAMEND)
out sph,temp
ldi temp, 0

;-----------------------

;INITIAL PROGRAMM

raz4:

ldi o___, 0
ldi a___, 0b00010000
eor o___, a___
out portc, o___
rcall delay_pause
in reg_cmp,pind ; считывание выходного сигнала компаратора Cmp
tst reg_cmp 
brne raz3 ; если reg_cmp==0 
ldi a___, 0b11101111
and o___, a___

raz3:

ldi a___, 0b00001000
eor o___, a___
out portc, o___
rcall delay_pause
in reg_cmp,pind ; считывание выходного сигнала компаратора Cmp
tst reg_cmp 
brne raz2 ; если reg_cmp==0 
ldi a___, 0b11110111
and o___, a___

raz2:

ldi a___, 0b00000100
eor o___, a___
out portc, o___
rcall delay_pause
in reg_cmp,pind ; считывание выходного сигнала компаратора Cmp
tst reg_cmp 
brne raz1 ; если reg_cmp==0 
ldi a___, 0b11111011
and o___, a___

raz1:

ldi a___, 0b00000010
eor o___, a___
out portc, o___
rcall delay_pause
in reg_cmp,pind ; считывание выходного сигнала компаратора Cmp
tst reg_cmp 
brne raz0 ; если reg_cmp==0 
ldi a___, 0b11111101
and o___, a___

raz0:

ldi a___, 0b00000001
eor o___, a___
out portc, o___
rcall delay_pause
in reg_cmp,pind ; считывание выходного сигнала компаратора Cmp
tst reg_cmp 
brne write ; если reg_cmp==0 
ldi a___, 0b11111110
and o___, a___

write:

out porta, o___
jmp raz4

delay_pause:

ldi t___, 20
ldi temp, 0
out tcnt0, temp

ccc:

in k___,tcnt0
cp k___,t___
brlo ccc

ret