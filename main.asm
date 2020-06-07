
LIST        p = p16f877a
#INCLUDE    <p16f877a.inc>	
__CONFIG    _HS_OSC & _PWRTE_OFF & _WDT_OFF & _BODEN_OFF & _CP_OFF & _LVP_ON; & _DP_OFF & _CPC_OFF

;==============================================
; DATA()
; Description: DATA
; Parameters: none
;==============================================
#DEFINE     FC_CHK_FQ   0x10                ; # of cycles to wait before check                                
                                            ; DATA HOLDERS:
dataLCD         EQU	    0x20                ; holds instructions/chars for LCD 
datacADC1       EQU	    0x21                ; holds the current AD sample, MSB
datacADC2	    EQU     0x22                ; holds the current AD sample, LSB
datapADC1       EQU	    0x23                ; holds the prev AD sample, MSB
datapADC2	    EQU     0x24                ; holds the prev AD sample, LSB
dataDAC1	    EQU     0x26                ; 4 bits MSB of the DAC sample
dataDAC2	    EQU     0x27                ; 8 bits LSB of the DAC sample
                                            ; CONFIGURATION HOLDERS:
sampleDACPtr    EQU     0x28                ; holds the ptr for the DAC sample
sampleADCPtr    EQU     0x29                ; holds the ptr for the ADC sample
crtMode         EQU     0x2A                ; current mode of operation
pushStatus      EQU     0x2B                ; prev mode btn status (pushed = 1)
fcselectH       EQU     0x2C                ; fc adjusters
fcselectL       EQU     0x2D                ;
fccheckf        EQU     0x2E                ; current 'fc update' interval
barValue        EQU     0X2F                ; display bar value
                                            ; INTERMEDIATES:
temp1           EQU     0X30                ; temps
temp2           EQU     0x31                ;
temp3           EQU     0x32                ;
counter1        EQU     0x33				; counters
counter2        EQU     0x34				;
       
ORG     0x00
            GOTO     MainInit               ; jump to main
ORG     0x05

;==============================================
; MAIN()
; Description: MAIN
; Parameters: none
;===============================================
MainInit:	BCF     STATUS, RP1	    ; go to bank 0
            BCF     STATUS, RP0	    ;
            CALL    InitPINS	    ; initialise pins as I/O
            CALL	InitLCD		    ; prepare LCD for first use
            CALL	InitADC		    ; configure ADC
            BSF     PORTD, RD3	    ; turn power LED on
            CLRF	pushStatus      ; set 'mode button' as released
            CLRF	crtMode         ; set current display to all pass
            BSF		crtMode, 2		;
            CALL	DisplayMode	    ; display the new mode
            MOVLW   FC_CHK_FQ       ; set the fc update interval
            MOVWF   fccheckf        ;
            MOVLW   0xFF            ;
            MOVWF   barValue
            GOTO    Start           ;

;==============================================
; Core()
; Description: Core
; Parameters: none
;===============================================
Start:      CALL    CheckFC         ; adjust fc
            BTFSC	PORTD, RD2	    ; if 'mode btn' pushed, clr prev 'mode btn'
            GOTO	PreSample	    ;     else
            BTFSC	pushStatus, 0   ; if prev 'mode btn' NOT pushed, change mode
            GOTO	SampleProc      ;     else, don't change mode
            RLF 	crtMode, 1	    ; get next mode
			BSF		pushStatus, 0	; set 'mode btn' as prev pushed
            BTFSS	crtMode, 3      ; if mode overflow, reset mode to 1
           	GOTO	UpdateMode      ;     else skip to mode display
            MOVLW	0x01            ; set mode to 1
            MOVWF	crtMode		    ; 
UpdateMode: CALL	DisplayMode	    ; display the new mode
            GOTO	SampleProc      ; continue processing
PreSample:  CLRF	pushStatus      ; set prev 'mode btn' as released
SampleProc: MOVF    datacADC1, 0    ; copy cADC into pADC
            MOVWF   datapADC1       ;
            MOVF    datacADC2, 0    ;
            MOVWF   datapADC2       ;
            MOVLW	0x21		    ; setup ADC sample pointer (cADC)
            MOVWF	sampleADCPtr    ; 
            CALL	GetADCSample	; get ADC sample
            CALL	FilterSample	; filter sample, with result in DAC
            MOVLW	0x26		    ; setup DAC sample pointer
            MOVWF	sampleDACPtr    ;
            CALL	SendDACSample	; send the output sample
            GOTO    Start		    ; repeat

;==============================================
; SendCmdLCD (dataLCD)
; Description: Sends instructions to LCD
; Parameters: dataLCD -> instruction
;===============================================
SendCmdLCD:
            CALL    Delay15ms	    ; wait 15 ms
    		BCF 	PORTD, 0	    ; E = 0, disable LCD (just to make sure)
    		BCF     PORTD, 1	    ; RS = 0, LCD data register holds an instr
    		MOVF	dataLCD, 0	    ; WREG = the INSTRUCTION
    		BSF     PORTD, 0	    ; E = 1, enable LCD (rising edge)
    		NOP                     ; wait 200 ns
			NOP                     ; wait 200 ns
    		MOVWF	PORTB		    ; write WREG to LCD data register
    		NOP                     ; wait 200 ns
			NOP                     ; wait 200 ns
    		BCF     PORTD, 0	    ; E = 0, disable LCD (falling edge)
            RETURN
    
;==============================================
; SendCharLCD (dataLCD)
; Description: Sends characters to LCD
; Parameters: dataLCD -> character
;===============================================
SendCharLCD:
            CALL    Delay15ms	    ; wait 15 ms
            BCF     PORTD, 0	    ; E = 0, disable LCD (just to make sure)
            BSF     PORTD, 1	    ; RS = 1, LCD data register now holds a char
            MOVF	dataLCD, 0	    ; WREG = the character
            BSF     PORTD, 0	    ; E = 1, enable LCD (rising edge)
            NOP                     ; wait 200 ns
			NOP                     ; wait 200 ns
            MOVWF	PORTB		    ; write WREG to LCD data register
            NOP                     ; wait 200 ns
			NOP                     ; wait 200 ns
            BCF     PORTD, 0	    ; E = 0, disable LCD (falling edge)
            RETURN

;==============================================
; GotoLine2 ()
; Description: Set DDRAM to row 1 (0x40 - 0x67, for N = 1)
; Parameters: none
;===============================================
GotoRow2:
            MOVLW    b'11000000'	    ; set address to 0x40
            MOVWF    dataLCD		    ;
            CALL     SendCmdLCD         ;
            RETURN

;==============================================
; DisplayBar ()
; Description: Display fc location on display
; Parameters: none
;==============================================
DisplayBar:
            GOTO    DBDone              ; not functional!
            DECF    barValue            ;
            SUBWF   barValue, 0         ;
            BTFSS   STATUS, Z           ;
            GOTO    DBDone              ;

            MOVLW   0xFF
            MOVWF   barValue            ;
            MOVF    fcselectH, 0        ;
            MOVWF   temp3               ;
            CALL    DisplayMode         ;
DBCheck:    DECFSZ  temp3               ;
            GOTO    DBDisplay           ;
            GOTO    DBDone              ;
DBDisplay:  MOVLW	'.'                 ; 
            MOVWF   dataLCD             ;
            CALL    SendCharLCD         ;
            GOTO    DBCheck             ;
DBDone:     RETURN                      ;

;==============================================
; DisplayMode (crtMode)
; Description: Displays the current mode
; Parameters: crtMode -> current mode (0 = LP, 1 = HP, 2 = AP)
;===============================================
DisplayMode:
            BTFSC	crtMode, 0      ; go to low pass for pin 0 set
            GOTO	DMlowPass       ;
            BTFSC   crtMode, 1      ; go to high pass for pin 1 set
            GOTO    DMhighPass      ;
            MOVLW   b'00000001'	    ; go to all pass for pin 2
            MOVWF   dataLCD		    ; (D7:D0) = 00000001
            CALL    SendCmdLCD	    ; DISPLAY CLEAR
            MOVLW	'A'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'l'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'l'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	' '             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'P'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'a'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	's'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	's'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            CALL	GotoRow2		;
            MOVLW	' '             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            GOTO	DMend           ;
DMhighPass: MOVLW   b'00000001'	    ; (D7:D0) = 00000001
            MOVWF   dataLCD		    ; DISPLAY CLEAR
            CALL    SendCmdLCD	    ;
            MOVLW	'H'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'i'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'g'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'h'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	' '             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'P'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'a'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	's'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
			CALL	GotoRow2		;
            MOVLW	's'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            GOTO	DMend           ;
DMlowPass:  MOVLW   b'00000001'	    ; (D7:D0) = 00000001
            MOVWF   dataLCD		    ; DISPLAY CLEAR
            CALL    SendCmdLCD	    ;
            MOVLW	'L'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'o'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'w'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	' '             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'P'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	'a'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	's'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            MOVLW	's'             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
            CALL	GotoRow2		;
            MOVLW	' '             ;
            MOVWF	dataLCD		    ;
            CALL	SendCharLCD	    ;
DMend:      RETURN

;==============================================
; Delay15ms ()
; Description: Delay of 15ms
; Parameters: none
;===============================================
Delay15ms:
            MOVLW   0x80			; WREG = 80;
            MOVWF   counter2		; counter2 = WREG;
d15loop1:  	MOVWF	counter1		; counter1 = WREG;
            DECF	counter2, 1		; counter2--;
            BTFSC	STATUS, Z		; if (counter2 != 0) then GOTO loop2;
            GOTO	d15finish		; else GOTO finish;
d15loop2:  	DECF	counter1, 1		; counter1--;
            BTFSS	STATUS, Z		; if (counter1 != 0) then GOTO loop2;
            GOTO	d15loop2		; else GOTO loop1;
            GOTO	d15loop1		; ----------------------------------------------------
d15finish: 	RETURN					;

;==============================================
; Delay19us ()
; Description: Delay of 19us
; Parameters: none
;===============================================
Delay19us:
            MOVLW   0x20			;
            MOVWF   counter1		;
d19loop:    DECFSZ  counter1, 1		;
            GOTO    d19loop			;
            RETURN

;==============================================
; Delay 25us ()
; Description: Delay of 25us
; Parameters: none
;===============================================
Delay25us:
            MOVLW   0x2A			;
            MOVWF   counter1		;
d25loop:    DECFSZ  counter1, 1		;
            GOTO    d25loop			;
            RETURN                  ;
            
;==============================================
; Delay Fcdelay ()
; Description: Delay for the selected fc
; Parameters: none
;===============================================
Fcdelay:
            MOVF    fcselectL, 0    ; LSB delay
            MOVWF   counter2		;
fc8loop:    CALL    Delay25us       ;
            DECFSZ  counter2, 1		;
            GOTO    fc8loop			;
bit9wait:   BTFSS   fcselectH, 0    ;
            GOTO    bit10wait       ;
            MOVLW   0xFF            ; 9th bit delay
            MOVWF   counter2		;
fc9loop:    CALL    Delay25us       ;
            DECFSZ  counter2, 1		;
            GOTO    fc9loop			;
bit10wait:  BTFSS   fcselectH, 1    ;
            GOTO    fcddone         ;
            MOVLW   0xFF            ; 10th bit delay (1)
            MOVWF   counter2		;
fc101loop:  CALL    Delay25us
            DECFSZ  counter2, 1		;
            GOTO    fc101loop		;
            MOVLW   0xFF            ; 10th bit delay (2)
            MOVWF   counter2		;
fc102loop:  CALL    Delay25us
            DECFSZ  counter2, 1		;
            GOTO    fc102loop	    ;
fcddone:    RETURN
            
;==============================================
; Delay CheckFC ()
; Description: Delay for the selected fc
; Parameters: none
;===============================================
CheckFC:
            MOVF    fccheckf, 0     ; 
            ANDLW   0xFF            ;     
            BTFSC   STATUS, Z       ; if fc check ready, get reading
            GOTO    fccdone         ;     else, decrease fc check
            DECFSZ  fccheckf        ; if fc check ready, get reading
            GOTO    fccdone         ;     else, do not change the fc
            BSF     STATUS, RP0	    ; change to bank 1
            MOVLW   b'10000001'	    ; set ADFM bit to RIGHT justify (8 bits in ADRESL)
            MOVWF   ADCON1		    ; Vref- = Vss, Vref+ = pin 5 (AN3)
            BCF     STATUS, RP0	    ; change to bank 0
            MOVLW   b'10110001'	    ; set adc clock to fosc/32
            MOVWF   ADCON0		    ; channel 6 (AN6 i.e.PIN9) and power up ADC
            MOVLW	0x2C		    ; setup sample pointer (2)
            MOVWF	sampleADCPtr    ; 
            CALL    GetADCSample    ; 
            CALL    InitADC         ; reconfigure adc
            MOVLW   FC_CHK_FQ       ; set the fc select interval
            MOVWF   fccheckf        ;
fccdone:    RETURN                  ;

;==============================================
; FilterSample (crtMode)
; Description: Filters the sample
; Parameters:  crtMode -> current Mode
;===============================================
FilterSample:
            BTFSC	crtMode, 0      ; 
            GOTO	FSlowPass       ; goto low pass as pin 0 set
            BTFSC   crtMode, 1      ; 
            GOTO    FShighPass      ; goto high pass as pin 1 set
            GOTO    FSCopy          ; else all pass
FSlowPass:  MOVF    datapADC2, 0    ; cADC2 -> cADC2 + pADC2
            ADDWF   datacADC2, 1    ; 
            BTFSC   STATUS, C       ; if overflow, cADC1++
            INCF    datacADC1, 1    ;
            MOVF    datapADC1, 0    ; cADC1 -> cADC1 + pADC1
            ADDWF   datacADC1, 1    ; 
            RRF     datacADC1, 1    ; cADC1 /= 2, and C = LSB of cADC1
            RRF     datacADC2, 1    ; cADC2 /= 2, and MSB = C
            MOVLW   b'00000011'     ;
            ANDWF   datacADC1, 1    ;
            CALL    Fcdelay
            CALL    DisplayBar
            GOTO    FSCopy          ; DAC -> cADC
FShighPass: CLRF    temp1           ;
            MOVF    datapADC2, 0    ; cADC2 -> ABS(cADC2 - pADC2)
            SUBWF   datacADC2, 1    ; 
            BTFSC   STATUS, C       ; if underflow, temp1++ and cADC2 = 255 - cADC2 
            GOTO    fshp1           ; 
            MOVF    datacADC2, 0    ; 
            SUBLW   0xFF            ; 
            MOVWF   datacADC2       ; 
            INCF    temp1           ;
fshp1:      MOVF    datapADC1, 0    ; cADC1 -> ABS(cADC1 - pADC1)
            SUBWF   datacADC1, 1    ;
            BTFSC   STATUS, C       ;if underflow, temp1++ and cADC1 = 255 - cADC1
            GOTO    fshp2           ;
            MOVF    datacADC1, 0    ;
            SUBLW   0xFF            ;
            MOVWF   datacADC1       ;
            INCF    temp1           ;
fshp2:      BTFSC   temp1, 0        ;if temp1 == 1 and cADC1 != 0, cADC1--
            GOTO    PreFSCopy          ;    cADC2 -> ABS(cADC2-255)
            MOVLW   0xFF            ;
            ANDWF   datacADC1, 0    ;
            BTFSC   STATUS, Z       ;
            GOTO    PreFSCopy       ;
            DECF    datacADC1, 1    ;
            MOVLW   0xFF            ;
            SUBWF   datacADC2, 1    ;
PreFSCopy:  CALL    Fcdelay
            CALL    DisplayBar
            GOTO    FSCopy    
FSCopy:     MOVF    datacADC1, 0    ; DAC -> cADC
            MOVWF   dataDAC1        ;
            MOVF    datacADC2, 0    ;
            MOVWF   dataDAC2        ;   
            RLF     dataDAC2        ;
            RLF     dataDAC1        ;
            RLF     dataDAC2        ;
            RLF     dataDAC1        ;
            MOVLW   b'11111100'     ;
            ANDWF   dataDAC2, 1     ;
FSend:      RETURN                  ;

;==============================================
; GetADCSample (dataADC)
; Description: Initialises PIC pins
; Parameters:  sampleADCPtr -> sample pointer
;===============================================
GetADCSample:
            MOVF    sampleADCPtr, 0 ; get pointer
            MOVWF   FSR             ;
            BCF     INTCON, PEIE	; disable a-d conversion complete interupt
            CALL	Delay19us	    ; wait aquisition time
            BSF     ADCON0, GO      ; start conversion
gadcwait:   BTFSC   ADCON0, GO      ; wait for conversion to finish
            GOTO    gadcwait        ;
            MOVF    ADRESH, 0       ; get higher sample of data
            MOVWF   INDF	        ; new sample
            INCF    FSR, 1          ;
            BSF     STATUS, RP0     ; go to bank 1
            MOVF    ADRESL, 0       ; get lower sample of data
            MOVWF   INDF	        ; new sample
            BCF     STATUS, RP0     ; go to bank 0
            RETURN                  ; 

;==============================================
; SendDACSample (sampleDACPtr)
; Description: Initialises PIC pins
; Parameters: sampleDACPtr -> the pointer for the dual word
;===============================================
SendDACSample:    
            CALL    Delay19us           ; delay between samples
            MOVF	sampleDACPtr, 0     ; 
            MOVWF   FSR                 ; set up pointer for sample to send
            BSF     PORTC, 6            ; make load signal false
            BCF     PORTC, 4            ; make clock low
            SWAPF	INDF, 1             ; msb of the sample go to higher nibble
            MOVLW   0x4                 ; init bit count for dac driver
            MOVWF   counter1            ; 4 MSB bits
            CALL    SendDACHalfSample   ;
            MOVLW   0x8                 ; init bit count for dac driver
            MOVWF   counter1            ; 8 LSB bits
            INCF    FSR, 1              ; 
            CALL    SendDACHalfSample   ; send the rest of 8 bits
            NOP                         ; allow for dac to settle down
            BCF     PORTC, 6            ; set load signal true 
            NOP                         ; allow dac to settle down
            BSF     PORTC, 6            ; make load signal false
            RETURN                      ;

;==============================================
; SendDACHalfSample ()
; Description: Sends atomiCALLy counter1  bits to the DAC
; Parameters: none
;===============================================
SendDACHalfSample:
            BCF     STATUS, C       ; make sure that carry bit is clear
sdhsTest:   RLF     INDF, 1         ; move bit msb into carry
            BTFSS   STATUS, C       ; check the bit
            GOTO    sdhsZero        ; if bit is not 1, go to 0
            BSF     PORTC, 5        ; else, drive the DAC SDI pin high
            BSF     PORTC, 4        ; toggle clock to get falling edge
            NOP                     ;
            BCF     PORTC, 4        ; 
            DECFSZ  counter1, 1     ; test if there are any more bits left
            GOTO    sdhsTest        ; more bits to test
            GOTO    sdhsEnd         ; no more bits, so end
sdhsZero:   BCF     PORTC, 5        ; bit is 0, drive the DAC SDI pin Low
            BSF     PORTC, 4        ;
            NOP                     ;
            BCF     PORTC, 4        ; toggle clock to get falling edge
            DECFSZ  counter1, 1     ;
            GOTO    sdhsTest        ; test if there are any more bits left
sdhsEnd:    RETURN                  ;

;==============================================
; InitPINS ()
; Description: Initialises PIC pins
; Parameters: none
;===============================================
InitPINS:
            BSF     STATUS, RP0     ; go to bank 1
            MOVLW	b'01001100'	    ; port A as input
            MOVWF	TRISA		    ;
            CLRF	TRISB		    ; port B is output
            MOVLW	b'00000100'     ; port D pins 2 as input
            MOVWF	TRISD           ;
            CLRF	TRISC           ; port C as output
            BCF     STATUS, RP0     ; go to bank 0
            RETURN                  ;

;==============================================
; InitLCD ()
; Description: Initialises LCD
; Parameters: none
;===============================================
InitLCD:   
            MOVLW   b'00110000'	    ; (D7:D0) = 0011XXXX
            MOVWF   dataLCD		    ; Init part I
            CALL    SendCmdLCD	    ;
            MOVLW   b'00110000'	    ; (D7:D0) = 0011XXXX
            MOVWF   dataLCD		    ; Init part II
            CALL    SendCmdLCD	    ;
            MOVLW   b'00110000'	    ; (D7:D0) = 0011XXXX
            MOVWF   dataLCD		    ; Init part III
            CALL    SendCmdLCD	    ;
            MOVLW   b'00111000'	    ; (D7:D0) = 00111XXX, N = 1, F = 0
            MOVWF   dataLCD		    ; FUNCTION SET (8 LINE)
            CALL    SendCmdLCD	    ; 
            MOVLW   b'00001000'	    ; (D7:D0) = 00001000
            MOVWF   dataLCD		    ; DISPLAY OFF
            CALL    SendCmdLCD	    ;
            MOVLW   b'00000001'	    ; (D7:D0) = 00000001
            MOVWF   dataLCD		    ; DISPLAY CLEAR
            CALL    SendCmdLCD	    ;
            MOVLW   b'00000110'	    ; (D7:D0) = 00000110
            MOVWF   dataLCD		    ; ENTRY MODE SET
            CALL    SendCmdLCD	    ;
            MOVLW   b'00001100'	    ; (D7:D0) = 00001111, I/D = 0, S = 0
            MOVWF   dataLCD		    ; DISPLAY ON
            CALL    SendCmdLCD	    ;
            RETURN

;==============================================
; InitADC ()
; Description: Initialises the analog to digital converter
; Parameters: none
;===============================================
InitADC:
            BSF     STATUS, RP0	    ; change to bank 1
            MOVLW   b'10000001'	    ; clear ADFM bit to RIGHT justify (8 bits in ADRESL)
            MOVWF   ADCON1		    ; Vref- = Vss, Vref+ = pin 5 (AN3)
            BCF     STATUS, RP0	    ; change to bank 0
            MOVLW   b'10010001'	    ; set adc clock to fosc/32
            MOVWF   ADCON0		    ; channel 2 (AN2 i.e.PIN4) and power up ADC
            RETURN				    ;
end

