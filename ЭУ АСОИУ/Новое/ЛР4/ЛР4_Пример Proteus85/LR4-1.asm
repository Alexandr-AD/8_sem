.include "m16def.inc" ; подключение библиотеки для работы с ATmega16
.list ; включение листинга
.def temp=r16 ; определение главного рабочего регистра
.def k__z=r17
.def k___=r18
.def s___=r19
.def kold=r20
;--------------------------------------------
.cseg ; выбор сегмента программного кода
.org 0 ; установка текущего адреса на ноль
;--------------------------------------------
ldi temp,0x80 ; выключение компаратора
out acsr,temp
;--------------------------------------------
ldi temp,0x00 ; 0 --> temp
out ddrd,temp ; Назначаем порт rd на ввод (00000000 --> ddrd)
ldi temp,0xFF ; 0xff --> temp
out ddrb,temp ; Назначаем порт rb на вывод (11111111 --> ddrb)
out portd,temp
;---------------------------------------------
ldi temp, 0b101 ; Предделение 1024
out tccr0, temp
ldi temp, 135 ; Коррекция тактовой частоты
out osccal, temp
ldi temp,low(RAMEND) ; инициализация стека
out spl,temp
ldi temp,high(RAMEND)
out sph,temp
ldi temp, 0

ldi	kold, 0x00

read:
in	k___, pind
cp	k___, kold
breq	read
tst	kold
brne	remem
tst	k___
breq	remem
jmp	met

remem:
mov	kold, k___
jmp	read

met:
ldi k__z, 5
rcall subr_delay1
rcall subr_delay0
ldi k__z, 10
rcall subr_delay1
ldi k__z, 5
rcall subr_delay0
ldi k__z, 10
rcall subr_delay1
ldi k__z, 5
rcall subr_delay0
rcall subr_delay1
ldi k__z, 50
rcall subr_delay0
jmp remem
subr_delay1: ; "1" длится k__z тактов с предделением
ldi s___, 1 ; 1 --> s___
out portb, s___ ; s___ --> pb
out tcnt0, temp ; 0 --> tcnt0 Обнуление таймера
ccc1: ; повтор цикла
in k___, tcnt0 ; считали таймер
cp k___, k__z ; сравнили k__ и k__z
brlo ccc1 ; если k___<k__z, ушли в начало
ret ; конец подпрограммы subr_delay1
subr_delay0: ; "0" длится k__z тактов с предделением
ldi s___, 0 ; 0 --> s___
out portb, s___ ; s___ --> pb
out tcnt0, temp ; 0 --> tcnt0
ccc0: ; повтор цикла
in k___, tcnt0 ; считали таймер
cp k___, k__z ; сравнили k__ и k__z
brlo ccc0 ; если k___<k__z, ушли в начало
ret ; конец подпрограммы subr_delay
