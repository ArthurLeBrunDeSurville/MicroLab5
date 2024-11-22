;
; lab5.asm
;
; Created: 11/20/2024 8:12:36 AM
; Author : Admin
;


; Replace with your application code
.include "m324PAdef.inc"

.equ LCD_PORT = PORTB
.equ LCD_PORT_DIR = DDRB
.equ LCD_PORT_PIN = PINB

.equ LCD_RS = 0
.equ LCD_RW = 1
.equ LCD_EN = 2
.equ LCD_SW = 3
.equ LCD_D4 = 4
.equ LCD_D5 = 5
.equ LCD_D6 = 6
.equ LCD_D7 = 7

.equ ADC_PORT = PORTA
.equ ADC_PORT_DIR = DDRA
.equ ADC_PORT_PIN = PINA

.def LCD_DATA = R16



.org 0x0000
JMP RESET_HANDLER
.org 0x001A
JMP TIM1_CMP_ISRA
.org 0x0030
JMP ADC_ISR

.org 0x0100
FIRST_LINE:
.db "VIN =     " , 0
SECOND_LINE:
.db "GROUP 02" , 0

.dseg
ADC_data : .byte 2

.cseg
RESET_HANDLER :
LDI R20, HIGH(RAMEND)
OUT SPH, R20
LDI R20, LOW(RAMEND)
OUT SPL, R20


CALL USART_INIT
CALL TIMER1_INIT
CALL ADC_INIT

CALL LCD_INIT
LDI ZH, HIGH(FIRST_LINE)
LDI ZL, LOW(FIRST_LINE)
CALL LCD_SEND_STRING
LDI R16, 0x01
LDI R17, 0x00
CALL LCD_MOVE_CURSOR
LDI ZH, HIGH(SECOND_LINE)
LDI ZL, LOW(SECOND_LINE)
CALL LCD_SEND_STRING

SEI
// INSERT LCD HERE
MAIN :

RJMP MAIN

ADC_INIT :
PUSH R20
LDI R20,0x00
OUT ADC_PORT_DIR, R20
LDI R20, (1 << ADEN ) | (1 << ADIE )  | (1 << ADPS0 ) | (1 << ADPS1 )| (0 << ADPS2 )
STS ADCSRA, R20
LDI R20, (0 << REFS1 ) | (1 << REFS0 )  | (0 << MUX4 ) | (0 << MUX3 ) | (0 << MUX2 ) | (0 << MUX1 ) | (0 << MUX0 )
STS ADMUX, R20
LDI R20,  (1 << ADEN ) | (1 << ADIE )  | (1 << ADPS0 ) | (0 << ADPS1 )| (0 << ADPS2 ) | (1 << ADSC)
STS ADCSRA, R20

POP R20
RET

ADC_ISR :
PUSH R20
PUSH R30
PUSH R31

LDI ZL, LOW(ADC_data)
LDI ZH, HIGH(ADC_data)

LDS R20, ADCL
ST Z+, R20
LDS R20, ADCH
ST Z, R20
LDI R20,  (1 << ADEN ) | (1 << ADIE )  | (1 << ADPS0 ) | (1 << ADPS1 )| (0 << ADPS2 ) | (1 << ADSC)
STS ADCSRA, R20

POP R31
POP R30
POP R20
RETI

TIMER1_INIT :

PUSH R20
 
LDI R20, (1 << OCIE1A)
STS TIMSK1 , R20 ; enable timer 1 overflow interrupt

LDI R20, 0x00
STS TCNT1H, R20 ; FOR 1HZ
LDI R20, 0x00
STS TCNT1L, R20 ; FOR 1HZ

LDI R20, 0x7A
STS OCR1AH, R20
LDI R20, 0x12
STS OCR1AL, R20

LDI R20, 0x00
STS TCCR1A, R16
LDI R20, (1 << WGM12)| (1 << CS12)
STS TCCR1B, R20 ; CTC, 256 prescaler

POP R20

RET

USART_INIT:
PUSH R20
; SET BAUD RATE TO 9600 BPS WITH 8 MHZ CLOCK
LDI R20, 0X33
STS UBRR0L, R20
; SET FRAME FORMAT: 8 DATA BITS, NO PARITY, 1 STOP BIT
LDI R20, (1 << UCSZ01) | (1 << UCSZ00)
STS UCSR0C, R20
; ENABLE TRANSMITTER AND RECEIVER
LDI R20, (1 << RXEN0) | (1 << TXEN0)
STS UCSR0B, R20

POP R20
RET

;SEND OUT 1 BYTE IN R16
USART_SENDCHAR:
PUSH R17
; WAIT FOR THE TRANSMITTER TO BE READY

USART_SENDCHAR_WAIT:
LDS R17, UCSR0A
SBRS R17, UDRE0 ;CHECK USART DATA REGISTER EMPTY BIT
RJMP USART_SENDCHAR_WAIT
STS UDR0, R16 ;SEND OUT
POP R17
RET

;RECEIVE 1 BYTE IN R16
USART_RECEIVECHAR:
PUSH R17
; WAIT FOR THE TRANSMITTER TO BE READY
USART_RECEIVECHAR_WAIT:

LDS R17, UCSR0A
SBRS R17, RXC0 ;CHECK USART RECEIVE COMPLETE BIT
RJMP USART_RECEIVECHAR_WAIT

LDS R16, UDR0 ;GET DATA
OUT PORTA, R16

POP R17
RET


LCD_INIT:
; set up i/o
LDI R16, 0B11110111
OUT LCD_PORT_DIR, R16
CALL DELAY_10MS ; wait for the lcd to wake tf up
CALL DELAY_10MS

;initialization command
LDI R16, 0x02 ; return cursor to original position
CALL LCD_SEND_COMMAND
LDI R16, 0x28 ;enable 2 lines and 5x7 matrix
CALL LCD_SEND_COMMAND
LDI R16, 0x0E ;display on, cursor blinking
CALL LCD_SEND_COMMAND
LDI R16, 0x01 ;clears the screen
CALL LCD_SEND_COMMAND
LDI R16, 0x80 ;force cursor to first line
CALL LCD_SEND_COMMAND
RET

LCD_SEND_COMMAND:
PUSH R17
CALL LCD_WAIT_BUSY ;lcd cant receive command if busy
MOV R17,R16
ANDI R17, 0xF0 ; RS = 0 & RW = 0 to send command
OUT LCD_PORT, R17 ; send first nibble
NOP
NOP
SBI LCD_PORT, LCD_EN
NOP
NOP
CBI LCD_PORT, LCD_EN
SWAP R16
ANDI R16, 0xF0
OUT LCD_PORT, R16 ; send second nibble
SBI LCD_PORT, LCD_EN
NOP
NOP
CBI LCD_PORT, LCD_EN
POP R17
RET

LCD_SEND_DATA :
PUSH R17
CALL LCD_WAIT_BUSY
MOV R17, R16
ANDI R17, 0xF0 ; set RS = 1, RW = 0
ORI R17, 0x01
OUT LCD_PORT, R17 ;Send first nibble
NOP
SBI LCD_PORT, LCD_EN
NOP
CBI LCD_PORT, LCD_EN
NOP
SWAP R16
ANDI R16, 0xF0
ORI R16,0x01
OUT LCD_PORT, R16
SBI LCD_PORT, LCD_EN
NOP
CBI LCD_PORT, LCD_EN
POP R17
RET

LCD_WAIT_BUSY:
PUSH R16
LDI R16, 0B00000111 ; SET I/O
OUT LCD_PORT_DIR, R16
LDI R16, 0B11110010 ; set RS=0, RW=1 for read lcd data
OUT LCD_PORT , R16
NOP
LCD_WAIT_LOOP :
SBI LCD_PORT, LCD_EN
NOP
NOP
IN R16, LCD_PORT_PIN
CBI LCD_PORT, LCD_EN
NOP
SBI LCD_PORT, LCD_EN
NOP
NOP
CBI LCD_PORT, LCD_EN
NOP
ANDI R16, 0x80
CPI R16, 0x80
BREQ LCD_WAIT_LOOP
LDI R16, 0b11110111
OUT LCD_PORT_DIR, R16
LDI R16, 0b00000000
OUT LCD_PORT, R16
POP R16
 RET

LCD_MOVE_CURSOR :
CPI R16, 0x00
BRNE LCD_MOVE_CURSOR_SECOND
ANDI R17, 0x0F
ORI R17, 0X80
MOV R16, R17
CALL LCD_SEND_COMMAND
RET
LCD_MOVE_CURSOR_SECOND:
CPI R16, 0x01
BRNE LCD_MOVE_CURSOR_EXIT
ANDI R17, 0x0F
  ORI R17, 0xC0
MOV R16, R17
CALL LCD_SEND_COMMAND

LCD_MOVE_CURSOR_EXIT:
RET

LCD_SEND_STRING :
PUSH ZH
PUSH ZL
PUSH LCD_DATA

LSL ZL
ROL ZH
LCD_SEND_STRING_01:
LPM LCD_DATA, Z+
CPI LCD_DATA, 0x00
BREQ LCD_SEND_STRING_02
CALL LCD_SEND_DATA
RJMP LCD_SEND_STRING_01

LCD_SEND_STRING_02:
POP LCD_DATA
POP ZL
POP ZH
RET

DELAY_10MS :
PUSH R16
PUSH R17
LDI R17  ,0x50
LOOP_4 :
LDI R16  ,0xFA
LOOP_3  :
NOP
DEC R16
BRNE LOOP_3
DEC R17
BRNE LOOP_4

POP R17
POP R16
RET


TIM1_CMP_ISRA:

CALL DISPLAY_ADC_IN

RETI

DISPLAY_ADC_IN :
;A*Vref = A*5
LDS AL,ADCL
LDS AH,ADCH
LDI BL,LOW(5)       ;Load multiplier into BH:BL
    LDI BH,HIGH(5)      ;
CALL MUL16x16 ;Reusult is stored in: R21,R20

LDI R16, 0x00
LDI R17, 0x06
CALL LCD_MOVE_CURSOR
;V=x.--------------------------------------------------------------------------------
;A*5/1024
;Ketqua: R0:is the integer part, display it on LCD. R2,R3 is the remainder.
MOV AL,R20
MOV AH,R21
LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    LDI BH,HIGH(1024)      ;
CALL DIV1616

;display the integer part x._
MOV R16,R0
LDI R17,0x30
ADD R16,R17
CALL USART_SENDCHAR
CALL LCD_SEND_DATA

LDI R16,46 ; "."
CALL USART_SENDCHAR
CALL LCD_SEND_DATA
;V=x.x-------------------------------------------------------------------------------
;remainder*10
MOV AL,R2
MOV AH,R3
LDI BL,LOW(10)       ;Load multiplier into BH:BL
    LDI BH,HIGH(10)      ;
CALL MUL16x16 ;Reusult is stored in: R21,R20

;reamainder*10/1024
;R0:integer part, display it on LCD. R2,R3 is remainder.
MOV AL,R20
MOV AH,R21
LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    LDI BH,HIGH(1024)      ;
CALL DIV1616

;Display integer part _.x
MOV R16,R0
LDI R17,0x30
ADD R16,R17
CALL USART_SENDCHAR
CALL LCD_SEND_DATA
;V=x.xx-----------------------------------------------------------------------------
;So du*10
MOV AL,R2
MOV AH,R3
LDI BL,LOW(10)       ;Load multiplier into BH:BL
    LDI BH,HIGH(10)      ;
CALL MUL16x16

MOV AL,R20
MOV AH,R21
LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    LDI BH,HIGH(1024)      ;
CALL DIV1616

MOV R16,R0
LDI R17,0x30
ADD R16,R17
CALL USART_SENDCHAR
CALL LCD_SEND_DATA
;V=x.xxx---------------------------------------------------------------------------------

MOV AL,R2
MOV AH,R3
LDI BL,LOW(10)       ;Load multiplier into BH:BL
    LDI BH,HIGH(10)      
CALL MUL16x16

MOV AL,R20
MOV AH,R21
LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    LDI BH,HIGH(1024)      ;
CALL DIV1616

MOV R16,R0
LDI R17,0x30
ADD R16,R17
CALL USART_SENDCHAR
CALL LCD_SEND_DATA

LDI R16,0x0A
RCALL USART_SENDCHAR
LDI R16,0x0D
CALL USART_SENDCHAR
CALL LCD_SEND_DATA

LDI R16, 0x00
LDI R17, 0x0B
CALL LCD_MOVE_CURSOR
LDI R16,'V' ; "."
CALL USART_SENDCHAR
CALL LCD_SEND_DATA
RET

;-----------------------------------------------------------------------------
.DEF ZERO = R2               ;To hold Zero
/*.DEF   AL = R16              ;To hold multiplicand
.DEF   AH = R17
.DEF   BL = R18              ;To hold multiplier
.DEF   BH = R19*/
.DEF ANS1 = R20              ;To hold 32 bit answer
.DEF ANS2 = R21
.DEF ANS3 = R22
.DEF ANS4 = R23

    LDI AL,LOW(42)       ;Load multiplicand into AH:AL
    LDI AH,HIGH(42)      ;
    LDI BL,LOW(10)       ;Load multiplier into BH:BL
    LDI BH,HIGH(10)      ;

MUL16x16:
     CLR ZERO             ;Set R2 to zero
     MUL AH,BH            ;Multiply high bytes AHxBH
     MOVW ANS4:ANS3,R1:R0 ;Move two-byte result into answer

     MUL AL,BL            ;Multiply low bytes ALxBL
     MOVW ANS2:ANS1,R1:R0 ;Move two-byte result into answer

     MUL AH,BL            ;Multiply AHxBL
     ADD ANS2,R0          ;Add result to answer
     ADC ANS3,R1          ;
     ADC ANS4,ZERO        ;Add the Carry Bit

     MUL BH,AL            ;Multiply BHxAL
     ADD ANS2,R0          ;Add result to answer
     ADC ANS3,R1          ;
     ADC ANS4,ZERO        ;Add the Carry Bit
RET
;-----------------------------------------------------------------------------
.DEF ANSL = R0            ;To hold low-byte of answer
.DEF ANSH = R1            ;To hold high-byte of answer    
.DEF REML = R2            ;To hold low-byte of remainder
.DEF REMH = R3            ;To hold high-byte of remainder
.DEF   AL = R16           ;To hold low-byte of dividend
.DEF   AH = R17           ;To hold high-byte of dividend
.DEF   BL = R18           ;To hold low-byte of divisor
.DEF   BH = R19           ;To hold high-byte of divisor  
.DEF    C = R20           ;Bit Counter

      LDI AL,LOW(420)   ;Load low-byte of dividend into AL
      LDI AH,HIGH(420)  ;Load HIGH-byte of dividend into AH
      LDI BL,LOW(10)    ;Load low-byte of divisor into BL
      LDI BH,HIGH(10)   ;Load high-byte of divisor into BH
DIV1616:
      MOVW ANSH:ANSL,AH:AL ;Copy dividend into answer
      LDI C,17          ;Load bit counter
      SUB REML,REML     ;Clear Remainder and Carry
      CLR REMH          ;
LOOP:  
ROL ANSL          ;Shift the answer to the left
    ROL ANSH          ;
    DEC C             ;Decrement Counter
    BREQ DONE        ;Exit if sixteen bits done
    ROL REML          ;Shift remainder to the left
    ROL REMH          ;
    SUB REML,BL       ;Try to subtract divisor from remainder
    SBC REMH,BH
    BRCC SKIP        ;If the result was negative then
    ADD REML,BL       ;reverse the subtraction to try again
    ADC REMH,BH       ;
    CLC               ;Clear Carry Flag so zero shifted into A
    RJMP LOOP        ;Loop Back
SKIP:  
SEC               ;Set Carry Flag to be shifted into A
    RJMP LOOP
DONE:
RET
