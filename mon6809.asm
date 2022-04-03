	cpu	6809

;	Created at						20 jan  2021.
;	Scrumpel 8a and 8b monitor
;	device addresses.

sbuf			equ	$cf01		;ACIA Data register
scsr			equ	$cf00		;ACIA Status register
leds			equ	$cf20		;LEDS
aciainit		equ	%00010101	;8N1 @ 115200

;	pointers and storage.

more_ctr		equ	12
more_max		equ	13

mbyte			equ	$10
keycommand		equ	$11
keycharacter		equ	$12
counter			equ	$13
buffer			equ	$14
tobuf			equ	$16
crc			equ	$18
teller			equ	$19
movscr			equ	$1A

breakpoint		equ	$20
reg_base		equ	$22
rega			equ	reg_base
regb			equ	rega+1
regpsw			equ	regb+1
regx			equ	regpsw+1
regy			equ	regx+2
regdp			equ	regy+2
regup			equ	regdp+1
regsp			equ	regup+2
regpc			equ	regsp+2
tempstor		equ	regpc+2

consolebreak	    	equ	tempstor+2
old103d			equ	consolebreak+1
intvar			equ	old103d+1

;	special characters.

cr			equ	$d
lf			equ	$a
esc			equ	$1b
cls			equ	$0c
xon			equ	$11
xoff			equ	$13
space			equ	$20
bs			equ	8
ctlg			equ	7

irq_vector		equ	$0f3
xirq_vector		equ	$0f0
realtime_interrupt	equ	$0fc
ramswi			equ	$0a1
resetflag		equ	$06e


stackbase		equ	$bfff

	org	0
	phase	$c000

	jmp	start

jump_table

	jmp	in			;Serial input.
	jmp	inul			;Direkte seriele input.
	jmp	bytot			;Serial byte out.
	jmp	getnibble		;Serial nibble input.
	jmp	bytin			;Serial byte in.
	jmp	ot			;Serial output.
	jmp	ott			;Serial text out.
	jmp	xot			;Serial X out.
	jmp	return			;Return monitor.
	jmp	bitot			;Bitot <a>.
	jmp	adresin			;Serial adres in in X.
	jmp	crlf			;Serial carriage linefeed.
	jmp	mesgot			;Display message #(A) vanaf (X).
	pshs	d			;Delay (x) maal 3 microsecondes @ 8 MHz.
	bra	delop1

delay2	pshs	d			;	  cycles
	ldd	#4000			;	3 cycles
delop1	subd	#1			;	4 cycles
	bne	delop1			;	6 cycles Branch taken
					;	5 cycles Branch not taken
	puls	d			;	  cycles
	rts				;	5 cycles

ot	pshs	b			;Save B register
	ldb	more_ctr		;Get more counter value
	beq	no_more			;If zero, more is switched off
	cmpa	#lf			;Count lf
	bne	no_more
	dec	more_ctr		;Count more until zero
	bne	no_more
	pshs	x			;Save X
	ldx	#more_txt		;Point X to moretext
	jsr	ott			;Print it
	jsr	in			;Wait for a key pressed
	lda	#cr			;CR in A
	puls	x			;Get X back
no_more ldb	scsr			;Read ACIA status
	bitb	#$1			;Key pressed?
	beq	otlp			;No? Send character
	jsr	checkxoff		;Check if XOFF
otlp	ldb	scsr			;Read ACIA status
	bitb	#$2
	beq	otlp			;Transmitter not empty then wait
	sta	sbuf			;Transmit the character
	puls	b			;Restore B
	rts

checkxoff

	ldb	scsr
	bitb	#$1
	beq	exckff
	ldb	sbuf			;Get data
	andb	#$7f
	cmpb	#xoff			;Check for XOFF
	bne	chkbrk

	pshs	a			;Save A

wait_for_xon

	jsr	in			;Get a character
	cmpa	#xon			;Is it XON?
	bne	wait_for_xon		;NO? then wait
	puls	a			;Restore A

exckff	rts

chkbrk	cmpb	#esc			;Is ESC pressed?
	bne	exckff			;No then continue
	ldb	#0			;Set breakflag on BREAK
	stb	consolebreak
	bra	exckff			;Continue

more_txt

	fcb	lf,"--- more ---",0

ott	lda	0,x+			;Display text pointed by X and inc X.
	beq	exits			;Character=0? Then stop
	jsr	ot			;Print the character
	bra	ott			;Get next character
exits	rts

bytot	pshs	a			;Save A
	pshs	b			;Save B
	sta	mbyte			;Save hex-byte.
	ldb	#2			;Print two nibbles
	lsra
	lsra
	lsra
	lsra
bytlp	anda	#$0f			;Mask lower byte
	pshs	a			;Save A
	tfr	cc,a
	anda	#%11011100
	tfr	a,cc
	puls	a			;Restore A
	daa				;Decimal adjust
	adda	#$f0
	adca	#$40
	jsr	ot			;Print the HEX digit.
	lda	mbyte			;Restore hex-byte
	decb
	bne	bytlp			;Get next nibble
	puls	b			;Restore B
	puls	a			;Restore A
	rts

escape	ldx	#tesc			;Point X to esc text
	jsr	ott			;Print it
	jmp	return			;Return to monitor

inul	pshs	x			;Save X
	pshs	b			;Save B
	ldb	more_max
	stb	more_ctr
inlp	ldb	scsr			;Get ACIA status
	bitb	#$1
	beq	inlp			;Wait until character sent
	lda	sbuf			;Get character
	anda	#$7f			;Remove eight bit
	cmpa	#esc			;Check for esc key
	beq	escape			;Pressed? Goto escape
	puls	b			;Restore B
	puls	x			;Restore X
	rts

in	bsr	inul			;Get character
	cmpa	#'z'
	beq	conup
	bcs	nolc
	rts
conup	anda	#$5f			;Convert to uppercase
	rts
nolc	cmpa	#'a'
	beq	conup
	bcc	conup
	rts

getnibble

	jsr	in			;Get character
alnibin	cmpa	#'0'			;Is it a 0?
	bcs	error			;No, then error.
	cmpa	#$3a
	bcs	sub0
	cmpa	#'A'
	bcs	error
	cmpa	#'G'
	bcc	error
	jsr	ot			;Echo character
	suba	#7
	bra	add0
sub0	jsr	ot			;Echo character
add0	suba	#'0'
	andcc	#$FE			;Clear carry
	rts
error	orcc	#1			;Set carry
	rts

bytin	jsr	getnibble
	bcs	bytin
albytin	sta	mbyte
bytin1	jsr	getnibble
	bcs	bytin1
	tfr	a,b
	lda	mbyte
	lsla
	lsla
	lsla
	lsla
	pshs	b
	adda	,s+
	rts

adresin	jsr	bytin			;Get high byte of address
	pshs	a			;Save A
	jsr	bytin			;Get low byte of address
	tfr	a,b			;Save A into B
	puls	a			;Restore A
	exg	d,x			;Transfer D to X
	rts

xot	pshs	x			;Save X
	exg	d,x			;Transfer X to D
	pshs	b			;Save B
	jsr	bytot			;Print higher byte
	puls	a			;Restore in A
	jsr	bytot			;Print lower byte
	puls	x			;Restore X
	rts

ecls	ldx	#clscode
	jmp	ott

clscode	fcb	cls,0			;Clearscreen sequence for your terminal.

crlf	lda	#cr			;Display CR/LF.
	jsr	ot
	lda	#lf
	jmp	ot

softreset

	ldx	$FFFe
	jmp	0,x

montxt	fcb	"6809 Monitor version 1.0/SER working at the Scrumpel 8 SBC",cr,lf
	fcb	"development board, (c) 2021 by N. Brok the Netherlands.",cr,lf,0
prompt	fcb	cr,lf,"6809> ",0
tesc	fcb	cr,lf,"Escaped.",0
alt2	fcb	" : ",0

initr	clr	resetflag		;Clear resetflag form BASIC
	clr	more_max		;Clear MORE
	clr	more_ctr
	lda	#%00000011		;Reset ACIA
	sta	scsr
	clr	leds			;Clear the LEDS
	lda	#$ff
	sta	consolebreak		;No console break
	lda	#aciainit
	sta	scsr			;ACIA at 8N1 115200 baud

;** initialization from the system variables **

	ldx	#$0
	stx	breakpoint		;Clear breakpoint.
	lda	#$7e			;Initialize SWI vector.
	sta	ramswi
	ldx	#software_interrupt
	stx	ramswi+1
;	ldx	#movscr			;Helpinstructions for block.
;	lda	#$a6
;	sta	0,x
;	lda	#$a7
;	sta	2,x
;	lda	#$39
;	sta	4,x
	bsr	clear_reg		;Clear the registers
	rts

clear_reg

	ldx	#reg_base		;Clear the CPU-registers
	ldb	#$E
clrlp1	clr	0,x+
	decb
	bne	clrlp1			;All registers done?
	rts

start	lds	#stackbase		;Initialize stackpointer.
	jsr	initr			;Initialize the monitor.
	jsr	ecls			;Clear screen.
	ldx	#montxt			;Display welcome text.
	jsr	ott
reentry	ldx	#prompt			;Display prompt.
	jsr	ott

;Command interpreter.

cmdlp	jsr	in			;Get the command.
	cmpa	#cr
	beq	return
	cmpa	#space			;Ignore space and below
	bcs	cmdlp
	jsr	ot
	cmpa	#'?'			;? is short for help
	bne	cmdnxt
	jsr	help
	jmp	return
cmdnxt	pshs	a
	jsr	in
	jsr	ot
	tfr	a,b
	lda	#space
	jsr	ot
	puls	a
	std	intvar
	ldx	#command_table
nextcmd	lda	0,x			;Get command.
	ldb	1,x
	cmpd	#0
	beq	cerror			;Zero? then print errormessage.
	cmpd	intvar
	beq	ishem			;Found? Execute the command.
	leax	4,x			;No? The try next command.
					;Add 4 to X to get next command.
	bra	nextcmd			;Compare the next command.
ishem	leax	2,x			;Add two to X to get the address of the command.
	lda	0,x			;Get Hbyte from address.
	sta	buffer			;Save it into a 16 bits buffer.
	lda	1,x			;Get Lbyte from address.
	sta	buffer+1		;Save this into buffer.
	ldx	buffer			;Load X with address.
	jsr	0,x			;Jump to it as a subroutine.
	jmp	return			;Return to the monitor when returned.

cerror	ldx	#command_error_text	;Point X to errortext
merror	jsr	ott			;Display errortext.
	
return	lds	#stackbase		;Initialize the stackpointer.
	bra	reentry			;Back to prompt.

more_cmd

	jsr	bytin			;Get number of lines.
	sta	more_max		;Insert a pause after this.
	sta	more_ctr
	jmp	crlf

software_interrupt

;	Execute breakpoint.

	ldx	#break_ex_txt
	jsr	ott			;Display "breakpoint executed".
	puls	a
	sta	regpsw			;Get the registers from stack.
	puls	a
	sta	rega
	puls	a
	sta	regb
	puls	a
	sta	regdp
	puls	x
	stx	regx
	puls	x
	stx	regy
	puls	x
	stx	regup
	puls	x
	leax	-1,x			;PC points to the SWI instruction.
	stx	regpc
	tfr	s,x
	lda	,-x			;Correction of the stackpointer.
	stx	regsp
	jsr	disreg	
	ldx	breakpoint		;Restore byte at breakpoint.
	lda	tempstor
	sta	0,x			;Restore orginal byte.
	ldx	#$0
	stx	breakpoint		;Clear breakpoint.
	bra	return			;Reinitialize Stackpointer.

break_ex_txt

	fcb	"Breakpoint executed.",cr,lf,0

showreg

disreg	jsr	crlf			;Print empty line.
	ldx	#regtxt			;Display the registers.
	jsr	ott
	lda	rega
	jsr	bytot
	lda	#space
	jsr	ot
	lda	regb
	jsr	bytot
	lda	#space
	jsr	ot
	jsr	ot
	lda	regpsw
	jsr	bitot
	lda	#space
	jsr	ot
	jsr	ot
	ldx	regx
	jsr	xot
	lda	#space
	jsr	ot
	ldx	regy
	jsr	xot
	lda	#space
	jsr	ot
	ldx	regsp
	jsr	xot
	lda	#space
	jsr	ot
	ldx	regup
	jsr	xot
	lda	#space
	jsr	ot
	lda	regdp
	jsr	bytot
	lda	#space
	jsr	ot
	jsr	ot
	ldx	regpc
	jsr	xot
	jmp	crlf

regtxt	fcb	" a  b   E F H I N Z V C   ix   iy   sp   up  dp   pc",cr,lf,0

bitot	ldb	#8			;We have 8 bits to process.
bitnx	asla				;One bit to the left, via carry.
	pshs	a			;Save A.
	bcs	bit1			;If carry is set, display an '1'.
bit0	lda	#space
	jsr	ot
	lda	#'0'			;Otherwise display a '0'
	bra	bitn
bit1	lda	#space
	jsr	ot
	lda	#'1'
bitn	jsr	ot
	puls	a
	decb				;Next bit.
	bne	bitnx
	rts

setbreakpoint

	jsr	adresin			;Get breakpointaddress.
	stx	breakpoint
	lda	0,x
	sta	tempstor		;Save original byte.
	lda	#$3f			;Place SWI instruction.
	sta	0,x
	rts
	
command_error_text

	fcb	" Unknown command type ? or HE for help.",0

command_table

	fcb	"AM"
	fdb	alter			;Alter memory command.
	fcb	"BC"
	fdb	copy			;Blockcopy command.
	fcb	"BD"
	fdb	delete			;Blockdelete command.
	fcb	"BI"
	fdb	insert			;Blockinsert command.
	fcb	"CS"
	fdb	ecls			;Clear screen command.
	fcb	"FI"
	fdb	snuffel			;Search command.
	fcb	"GO"
	fdb	goto			;Goto command.
	fcb	"HD"
	fdb	hdump			;Hexdump command.
	fcb	"AR"
	fdb	modifyreg		;Modify registers command.
	fcb	"MO"
	fdb	more_cmd		;More command
	fcb	"PM"
	fdb	preset			;Preset command.
	fcb	"SR"
	fdb	showreg			;Showregisters command.
	fcb	"SB"
	fdb	setbreakpoint		;Set breakpoint command.
	fcb	"TI"
	fdb	transfer_in		;Transferinput command. (INTEL HEX!)
	fcb	"TO"
	fdb	transfer_ot		;Transferoutput command.
	fcb	"HE"
	fdb	help			;Help command.
	fdb	0
	
helptext

	fcb	cr,lf,"This is the 6809 Scrumpel 8 monitor command helpmenu."
	fcb	cr,lf,cr,lf,cr,lf
	fcb	"AM <Alter>               AR <Alter register>  BC <Block copy>",cr,lf
        fcb	"BD <Block delete>        BI <Block insert>    CS <Clear screen>",cr,lf
	fcb	"FI <Find>                GO <Goto>            HD <Hexdump>",cr,lf
	fcb	"MO <set more>            PM <Preset>          SB <Set breakpoint>",cr,lf
	fcb	"SR <Show registers>      TI <TransferInput>   TO <TransferOutput>",cr,lf
	fcb	"HE <Help, this menu>      ? <Help, this menu>",cr,lf
	fcb	cr,lf,cr,lf
	fcb	"For more details see the 6809 monitor users guide.",cr,lf,0

help	jsr	ecls			;Clear display.
	ldx	#helptext		;Point X to helptext.
	jmp	ott			;Display the text.

goto	jsr	adresin			;Get address.
	jsr	in			;Wait for key.
	jsr	crlf			;Display crlf.
	ldy	#return			;Put returnaddress onto stack.
	pshs	y
	pshs	x			;Set the goto address onto stack.
	ldx	breakpoint		;Breakpoint set?
	beq	goto1			;No then normal goto.
	lda	regpsw
	tfr	a,cc			;Set psw.
	lda	rega			;No? get the registers.
	ldb	regb
	ldx	regx
	ldy	regy
goto1	rts

modifyreg

	clr	counter
	ldy	#reg_base
reglp1	jsr	crlf
	lda	counter
	jsr	dregot
	lda	0,y
	pshs	a
	jsr	bytot
	lda	#space
	jsr	ot
	puls	a
	jsr	asciiot
	ldx	#alt2
	jsr	ott
reglp4	jsr	in
	cmpa	#cr
	beq	plusreg
	cmpa	#'-'
	beq	minreg
	jsr	alnibin
	bcs	reglp4
	jsr	albytin
	sta	0,y
	bra	plsreg
plusreg	lda	#'+'
	jsr	ot
plsreg	leay	1,y
	inc	counter
	cmpy	#regsp+2
	beq	modifyreg
	bra	reglp1

minreg	jsr	ot
	leay	-1,y
	dec	counter
	cmpy	#reg_base-1
	beq	modifyreg
	bra	reglp1


regtab	fcb	"ACCA=",0
	fcb	"ACCB=",0
	fcb	" CCR=",0
	fcb	" IXh=",0
	fcb	" IXl=",0
	fcb	" IYh=",0
	fcb	" IYl=",0
	fcb	"  DP=",0
	fcb	" UPh=",0
	fcb	" UPl=",0
	fcb	" SPH=",0
	fcb	" SPL=",0

dregot	ldx	#regtab
mesgot	pshs	b
	tsta
	beq	mesdi
meslp1	ldb	0,x+
	tstb
	bne	meslp1
	deca
	bne	meslp1
mesdi	jsr	ott
	puls	b
	rts

asciiot	anda	#$7f			;Reset bit 8 from character.
	cmpa	#$7e			;Unprintable character?
	bcc	otpunt			;Print a '.'.
	cmpa	#' '			;Unprintable character?
	bcc	otkar			;No, print the character.
otpunt	lda	#'.'			;Print a '.'.
otkar	jmp	ot

alterg	jsr	ot			;Entry for G command.
	lda	#space			;Print a space.
	jsr	ot

alter	jsr	adresin			;Get address.
altl0	jsr	crlf
	jsr	xot			;Display address in X.
	lda	#space			;Display a space.
	jsr	ot
	lda	0,x			;Get byte pointed by address.
	pshs	a			;Save it.
	jsr	bytot			;Display byte.
	lda	#space			;Display space.
	jsr	ot
	puls	a			;Restore A.
	jsr	asciiot			;Display this as an ASCII character.
	pshs	x			;Save X.
	ldx	#alt2			;Display " : ".
	jsr	ott
	puls	x
altla	jsr	in			;Get first character
	cmpa	#cr			;CR?
	beq	plus			;Yes? Then next address
	cmpa	#'-'			;-?
	beq	min			;Yes? Then previous address
	cmpa	#'G'			;G?
	bne	alnxt			;No? Then check further
	jmp	alterg			;Yes? Then ask for new address
alnxt	cmpa	#'R'			;R?
	beq	relative		;Yes? The calculate relative address
	cmpa	#"'"			; '?
	beq	txtin			;A ' means text input
	jsr	alnibin			;It must be a hex digit
	bcs	altla			;No hex digit? Get first character again
	jsr	albytin			;Get rest of byte
	sta	0,x			;Store it on address pointed by X
	bra	plus1			;Increment address
plus	lda	#'+'			;Print a +
	jsr	ot
plus1	lda	,x+			;Increment X by one
	bra	altl0
min	jsr	ot			;Echo the character
	leax	-1,x			;Decrement X by one
altl0a	bra	altl0
txtin	jsr	ot			;Echo command
	stx	buffer			;Save beginaddress into buffer
txtinl	jsr	inul			;Get character in upper or lower case
	cmpa	#$8			;A backspace pressed?
	bne	txtver			;Get next character
	cmpx	buffer			;At begin of text?
	beq	txtinl			;Yes? Then do nothing get next character
	pshs	x			;Save X
	ldx	#bstxt			;Point X to backspace a character
	jsr	ott			;Print it
	puls	x			;Restore X
	leax	-1,x			;Decrement X one character
	bra	txtinl			;Get next character
txtver	cmpa	#' '			;Below space?
	bcs	txtinl			;Yes? Do nothing with it
	jsr	ot			;Echo character
	cmpa	#"'"			;A ' means end of text
	beq	altl0a
	sta	0,x+			;Store char and increment to next address
	bra	txtinl			;Get next character

relative

	jsr	ot			;Echo the command
	leax	1,x			;Increment X
	stx	buffer			;Save X in buffer
	lda	#space			;Print a space
	jsr	ot
	jsr	adresin			;Get branch to address
	stx	tobuf			;Save it in tobuf
	ldb	tobuf
	lda	tobuf+1
	suba	buffer+1
	sbcb	buffer
	beq	fwrd
	cmpb	#$ff
	beq	back
offerr	ldx	#offseterr		;Point X to offseterror text
	jsr	ott
	ldx	buffer			;Restore X from buffer
	leax	-1,x			;Decrement it
	jmp	altl0			;Try again

back	tsta
	bpl	offerr
	bra	oexit
fwrd	tsta
	bmi	offerr
oexit	ldx	buffer			;Get original address
	leax	-1,x			;Decrement it by one
	sta	0,x			;Save calculated offset in address
	pshs	a			;Save A
	lda	#'='			;Print =
	jsr	ot
	puls	a			;Restore A
	jsr	bytot			;Print the calculated offset
	jmp	plus1			;Gotot next address

offseterr

	fcb	" Relative offset out of range.",0

bstxt	fcb	bs,space,bs,0

hdump	jsr	adresin			;Get beginaddress to dump.
hloop	jsr	crlf
	pshs	x
	jsr	xot
	lda	#space
	jsr	ot
	ldb	#$8
hloop1	lda	#space
	jsr	ot
	lda	0,x+
	jsr	bytot
	lda	0,x+
	jsr	bytot
	decb
	bne	hloop1
	puls	x
	lda	#space
	jsr	ot
	ldb	#$10
hloop2	lda	0,x+
	jsr	asciiot
	decb
	bne	hloop2
	lda	consolebreak		;Check for break.
	bne	hloop
	lda	#$ff			;Stop dumping.
	sta	consolebreak
	rts

preset	jsr	adresin
	pshs	x
	lda	#space
	jsr	ot
	jsr	adresin
	leax	1,x
	stx	buffer
	puls	x
	lda	#space
	jsr	ot
	jsr	bytin
presl1	sta	0,x+
	cmpx	buffer
	bne	presl1
	rts

delete	jsr	bytin
	sta	movscr
	lda	#space
	jsr	ot
	jsr	adresin
	stx	buffer
	lda	#space
	jsr	ot
	jsr	adresin
	leax	1,x
	stx	tobuf
	ldx	buffer
dloop	pshs	x
	ldb	movscr
	abx
	lda	0,x
	puls	x
	sta	0,x+
	cmpx	tobuf
	bne	dloop
	rts

insert	jsr	bytin
	sta	movscr
	lda	#space
	jsr	ot
	jsr	adresin
	leax	-1,x
	stx	tobuf
	lda	#space
	jsr	ot
	jsr	adresin
iloop	lda	0,x
	pshs	x
	ldb	movscr
	abx
	sta	0,x
	puls	x
	leax	-1,x
	cmpx	tobuf
	bne	iloop
	rts

copy	jsr	adresin
	pshs	x
	lda	#space
	jsr	ot
	jsr	adresin
	leax	1,x
	stx	tobuf
	lda	#space
	jsr	ot
	jsr	adresin
	pshs	x
	puls	y
	puls	x
cloop	lda	0,x+
	sta	0,y+
	cmpx	tobuf
	bne	cloop
	rts

chksum	adda	crc			;Calculate checksum.
	sta	crc
	rts

transfer_in

	jsr	crlf
trl1	jsr	in
	cmpa	#':'
	bne	trl1
	jsr	ot
	jsr	bytin
	beq	trend
	sta	teller
	clr	crc
	jsr	chksum
	jsr	bytin
	sta	buffer
	jsr	chksum
	jsr	bytin
	sta	buffer+1
	jsr	chksum
	ldx	buffer
	jsr	bytin
	jsr	chksum
trl0	jsr	bytin
	sta	0,x+
	jsr	chksum
	dec	teller
	bne	trl0
	jsr	bytin
	nega
	cmpa	crc
	beq	transfer_in
chkerr	ldx	#transfererr
	jmp	ott
trend	jsr	in
	cmpa	#cr
	bne	trend
	rts

transfererr

	fcb	cr,lf,"CRC error in Intel-hex transfer.",0

transfer_ot

	jsr	adresin
	pshs	x
	lda	#space
	jsr	ot
	jsr	adresin
	stx	tobuf
	puls	x
tril1	clr	crc
	jsr	crlf
	stx	buffer
	lda	#':'
	jsr	ot
	lda	#$10
	sta	teller
	jsr	bytot
	jsr	chksum
	lda	buffer
	jsr	bytot
	jsr	chksum
	lda	buffer+1
	jsr	bytot
	jsr	chksum
	clra
	jsr	bytot
	jsr	chksum
tril0	lda	0,x+
	jsr	bytot
	jsr	chksum
	dec	teller
	bne	tril0
	lda	crc
	nega
	jsr	bytot
	pshs	x
	ldx	tobuf
	leax	-1,x
	beq	trend1
	stx	tobuf
	puls	x
	bra	tril1
trend1	puls	x
	ldx	#trendt
	jmp	ott

trendt	fcb	cr,lf,":00000001FF",cr,lf,0

snuffel	jsr	adresin
	pshs	x
	lda	#space
	jsr	ot
	jsr	adresin
	leax	1,x
	stx	tobuf
	puls	x
	lda	#space
	jsr	ot
	jsr	bytin

snuffel_verder

	cmpx	tobuf
	beq	snuffel_niet_gevonden
	ldb	0,x+
	pshs	b
	cmpa	,s+
	bne	snuffel_verder
	pshs	x
	ldx	#snuffel_gevonden_text
	jsr	ott
	puls	x
	leax	-1,x
	jmp	xot

snuffel_niet_gevonden

	ldx	#snuffel_niet_gevonden_text
	jmp	ott

snuffel_gevonden_text

	fcb	" found at : ",0

snuffel_niet_gevonden_text

	fcb	" not found in given memory block.",0

;	For more information about this monitor contact:
;	Nick brok Berliozlaan 1 5654 SM Eindhoven.

	dephase
	end
