; Using ADC to measure voltage and display it on LCD


.EQU	ADC_PORT=PORTA
.EQU	ADC_DR=DDRA
.EQU	ADC_IN=PINA	
.equ LCDPORT = PORTB ; Set signal port reg to PORTB
.equ LCDPORTDIR = DDRB ; Set signal port dir reg to PORTB
.equ LCDPORTPIN = PINB ; Set clear signal port pin reg to PORTB
.equ LCD_RS = PINB0
.equ LCD_RW = PINB1
.equ LCD_EN = PINB2
.equ LCD_D7 = PINB7
.equ LCD_D6 = PINB6
.equ LCD_D5 = PINB5
.equ LCD_D4 = PINB4 	
.ORG	0
RJMP	MAIN
.ORG	0X40

MAIN:	
	LDI		R16,HIGH(RAMEND)
	OUT		SPH,R16
	LDI		R16,LOW(RAMEND)
	OUT		SPL,R16

	// Configure UART, configure the ADC pins as input, configure other in/out pins as per the requirements.
	LDI R16, 0x00
	OUT ADC_DR, R16
	CALL USART_INIT
	// Configure the timer, select the appropriate timer and mode, and configure the auto trigger settings.
	// Timer 1 overflow. Prescaler 256. TCNT = 34285 (65535-31250) 0x85ED
	LDI R16, (1<<CS12)
	STS TCCR1B, R16
	LDI R16, 0x85
	STS TCNT1H, R16
	LDI R16, 0xED
	STS TCNT1L, R16
	// Configure the ADMUX register for the ADC, set Vref to Avcc = 5V, select single-ended ADC1, gain x1, and shift the result right.  
	LDI		R16, (1<<REFS0)
	STS		ADMUX,R16		
	
// Enable ADC operation: Bit 7 must be set. Bit 6 must be cleared. Enable auto-trigger mode. Set the ADC conversion complete flag (bit 4). 
// Disable interrupts (bit 3), and select the ADC clock prescaler (bits 2..0).    
	LDI R16, (1<<ADEN)|(1<<ADATE)|(1<<ADIF)
	STS ADCSRA, R16

// Configure ADCSRB register. Select the appropriate trigger source for auto-trigger. 
// Trigger source : Timer1 overflow
	LDI R16, (1<<ADTS2)|(1<<ADTS1)
	STS ADCSRB, R16
// Configure the DIDR0 register. Write 1 to disable digital mode on the ADC pins to save power. 
	LDI R16, 0b11111110
	STS DIDR0, R16

LOOP_1:
	// Wait for the ADIF flag in ADCSRA to become 1, indicating that the conversion is complete.
	// Once conversion is complete, clear the ADIF flag and send data via UART as per the required format: 0x55 ADCH ADCL 0xFF.
	LDS R16, ADCSRA
	SBRS R16, ADIF
	RJMP LOOP_1
	; ADC process done
	CBR R16, 4
	STS ADCSRA, R16 ; clear ADIF
	LDI R16, 0x55
	CALL USART_SendChar ; Send 0x55
	LDS R16, ADCH
	CALL USART_SendChar ; Send ADCH
	LDS R16, ADCL
	CALL USART_SendChar ; Send ADCL
	LDI R16, 0xFF
	CALL USART_SendChar ; Send 0xFF
	CALL INIT_TIMER
	RJMP LOOP_1	

// Subroutines for UART communication

;CPU clock is 8Mhz
USART_Init:
    ; Set baud rate to 9600 bps with 8MHz clock
    LDI	R16, 103
    STS	UBRR0L, R16
	;set double speed
    LDI	R16,(1 << U2X0)
    STS	UCSR0A,R16
    ; Set frame format: 8 data bits, no parity, 1 stop bit
    LDI	R16,(1 << UCSZ01) | (1 << UCSZ00)
    STS	UCSR0C,R16
    ; Enable transmitter and receiver
    LDI	R16,(1 << RXEN0) | (1 << TXEN0)
    STS	UCSR0B,R16
    RET

;send out 1 byte in r16
USART_SendChar:
    PUSH R17
    ; Wait for the transmitter to be ready
    USART_SendChar_Wait:
    LDS	R17,UCSR0A
    SBRS R17,UDRE0		;check USART Data Register Empty bit
    RJMP USART_SendChar_Wait
    STS	UDR0,R16		;send out
    POP	R17
    RET

;receive 1 byte in r16
USART_ReceiveChar:
    PUSH R17
    ; Wait for the transmitter to be ready
    USART_ReceiveChar_Wait:
    LDS	R17,UCSR0A
    SBRS R17, RXC0	;check USART Receive Complete bit
    RJMP USART_ReceiveChar_Wait
    LDS	R16,UDR0		;get data
    POP	R17
    RET

INIT_TIMER:
	LDI R16, 0x85
	STS TCNT1H, R16
	LDI R16, 0xED
	STS TCNT1L, R16
	CBI TIFR1, TOV1
	RET
;---------------------------------------------

; Subroutine to send command to LCD
LCD_Init:
	; Set up data direction register for Port A
	ldi r16, 0b11110111 ; set PA7-PA4 as outputs, PA2-PA0 as output
	out LCDPORTDIR, r16
	; Send initialization sequence
	ldi r16, 0x02 ; Function Set: 4-bit interface
	call LCD_Send_Command
	ldi r16, 0x28 ; Function Set: enable 5x7 mode for chars
	call LCD_Send_Command
	ldi r16, 0x0E ; Display Control: Display OFF, Cursor ON
	call LCD_Send_Command
	ldi r16, 0x01 ; Clear Display
	call LCD_Send_Command
	ldi r16, 0x80 ; Clear Display
	call LCD_Send_Command
	ret
;Command code in r16
;LCD_D7..LCD_D4 connect to PA7..PA4
;LCD_RS connect to PA0
;LCD_RW connect to PA1
;LCD_EN connect to PA2
LCD_Send_Command:
	push r17
	call LCD_wait_busy ; check if LCD is busy
	mov r17,r16 ;save the command
	; Set RS low to select command register
	; Set RW low to write to LCD
	andi r17,0xF0
	; Send command to LCD
	out LCDPORT, r17
	nop
	nop
	; Pulse enable pin
	sbi LCDPORT, LCD_EN
	nop
	nop
	cbi LCDPORT, LCD_EN
	swap r16 
	andi r16,0xF0
	; Send command to LCD
	out LCDPORT, r16
	; Pulse enable pin
	sbi LCDPORT, LCD_EN
	nop
	nop
	cbi LCDPORT, LCD_EN
	pop r17
	ret

LCD_Send_Data:
	push r17
	call LCD_wait_busy ;check if LCD is busy
	mov r17,r16 ;save the command
	; Set RS high to select data register
	; Set RW low to write to LCD
	andi r17,0xF0
	ori r17,0x01
	; Send data to LCD
	out LCDPORT, r17
	nop
	; Pulse enable pin
	sbi LCDPORT, LCD_EN
	nop
	cbi LCDPORT, LCD_EN
	; Delay for command execution
	;send the lower nibble
	nop
	swap r16
	andi r16,0xF0
	; Set RS high to select data register
	; Set RW low to write to LCD
	andi r16,0xF0
	ori r16,0x01
	; Send command to LCD
	out LCDPORT, r16
	nop
	; Pulse enable pin
	sbi LCDPORT, LCD_EN
	nop
	cbi LCDPORT, LCD_EN
	pop r17
	ret

LCD_wait_busy:
	push r16
	ldi r16, 0b00000111 ; set PA7-PA4 as input, PA2-PA0 as output
	out LCDPORTDIR, r16
	ldi r16,0b11110010 ; set RS=0, RW=1 for read the busy flag
	out LCDPORT, r16
	nop
	LCD_wait_busy_loop:
		sbi LCDPORT, LCD_EN
		nop
		nop
		in r16, LCDPORTPIN
		cbi LCDPORT, LCD_EN
		nop
		sbi LCDPORT, LCD_EN
		nop
		nop
		cbi LCDPORT, LCD_EN
		nop
		andi r16,0x80 
		cpi r16,0x80
		breq LCD_wait_busy_loop
		ldi r16, 0b11110111 ; set PA7-PA4 as output, PA2-PA0 as output
		out LCDPORTDIR, r16
		ldi r16,0b00000000 ; set RS=0, RW=1 for read the busy flag
		out LCDPORT, r16
		pop r16
	ret



/*
;------------
;Display
DISPLAY_V:
;A*Vref = A*5
	LDS AL,ADCL
	LDS AH,ADCH
	LDI BL,LOW(5)       ;Load multiplier into BH:BL
    	LDI BH,HIGH(5)      ;
	RCALL MUL16x16	;Reusult is stored in: R21,R20
;V=x.--------------------------------------------------------------------------------
	;A*5/1024
	;Ketqua: R0:is the integer part, display it on LCD. R2,R3 is the remainder.
	MOV AL,R20
	MOV AH,R21
	LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    LDI BH,HIGH(1024)      ;
	RCALL DIV1616

	;display the integer part x._
	MOV		R16,R0
	LDI		R17,0x30
	ADD		R16,R17
	RCALL	USART_SendChar

	LDI		R16,46	; "."
	RCALL	USART_SendChar
;V=x.x-------------------------------------------------------------------------------
	;remainder*10
	MOV AL,R2
	MOV AH,R3
	LDI BL,LOW(10)       ;Load multiplier into BH:BL
   	 LDI BH,HIGH(10)      ;
	RCALL MUL16x16		;Reusult is stored in: R21,R20

	;reamainder*10/1024
	;R0:integer part, display it on LCD. R2,R3 is remainder.
	MOV AL,R20
	MOV AH,R21
	LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    	LDI BH,HIGH(1024)      ;
	RCALL DIV1616

	;Display integer part _.x
	MOV		R16,R0
	LDI		R17,0x30
	ADD		R16,R17
	RCALL	USART_SendChar
;V=x.xx-----------------------------------------------------------------------------
	;So du*10
	MOV AL,R2
	MOV AH,R3
	LDI BL,LOW(10)       ;Load multiplier into BH:BL
    	LDI BH,HIGH(10)      ;
	RCALL MUL16x16	

	MOV AL,R20
	MOV AH,R21
	LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    	LDI BH,HIGH(1024)      ;
	RCALL DIV1616

	MOV		R16,R0
	LDI		R17,0x30
	ADD		R16,R17
	RCALL	USART_SendChar
;V=x.xxx---------------------------------------------------------------------------------

	MOV AL,R2
	MOV AH,R3
	LDI BL,LOW(10)       ;Load multiplier into BH:BL
    	LDI BH,HIGH(10)      
	RCALL MUL16x16	

	MOV AL,R20
	MOV AH,R21
	LDI BL,LOW(1024)       ;Load multiplier into BH:BL
    	LDI BH,HIGH(1024)      ;
	RCALL DIV1616

	MOV		R16,R0
	LDI		R17,0x30
	ADD		R16,R17
	RCALL	USART_SendChar

	LDI		R16,0x0A
	RCALL	USART_SendChar	
	LDI		R16,0x0D
	RCALL	USART_SendChar
RET

;-----------------------------------------------------------------------------
.DEF ZERO = R2               ;To hold Zero
.DEF   AL = R16              ;To hold multiplicand
.DEF   AH = R17
.DEF   BL = R18              ;To hold multiplier
.DEF   BH = R19
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
LOOP:   ROL ANSL          ;Shift the answer to the left
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
SKIP:   SEC               ;Set Carry Flag to be shifted into A
         RJMP LOOP
DONE:RET
*/
