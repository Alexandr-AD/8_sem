.include "m16def.inc"	; подключение библиотеки для работы с ATmega16
.list 					; включение листинга
.def	temp=r16		; определение главного рабочего регистра
.def	reg_cmp=r17		
.def	z___=r18		
.def	d___=r19		
.def	k___=r20
.def	t___=r21
.def	res___=r22
.cseg					; выбор сегмента программного кода
.org	0				; установка текущего адреса на ноль
;-----------------------
ldi		temp,0x80		; выключение компаратора
out 	acsr,temp		
;-----------------------
ldi		temp,0x00 		; 0 --> temp
out 	ddrd,temp 		; Назначаем порт rd на ввод (00000000 --> ddrd)
ldi		temp,0xFF 		; 0xff --> temp
out		ddrc,temp		
out		ddra,temp		
;-----------------------
ldi		temp,0b110		; Предделение 1024
out		tccr0,temp
ldi 	temp,low(RAMEND) ; инициализация стека
out 	spl,temp
ldi 	temp,high(RAMEND)
out 	sph,temp
ldi 	temp, 0 
;-----------------------

read:

in		reg_cmp,pind	; считывание выходного сигнала компаратора Cmp
tst		reg_cmp			;
breq 	reset			; если reg_cmp==0 

in		res___,tcnt0	; иначе сохранить значение таймера в res___
jmp		read			; Вернулись в read



reset:					

rcall 	start_1_func	; 1 - > Start
rcall delay_discharge	
out		tcnt0,temp		; 0 -> Timer
rcall	start_0_func	; o - > Start
out		porta,res___	; сохранить значение цифры
jmp		read			; Вернулись в read


delay_discharge:
in		t___,tcnt0 ; Timer - > T
ccc:
in		k___,tcnt0
cp		k___,t___
breq	ccc
ret

start_0_func:
ldi		z___,0
out		portc,z___
ret

start_1_func:
ldi		z___,1
out		portc,z___
ret
