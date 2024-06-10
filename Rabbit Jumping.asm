[org 0x0100]
jmp start_screen
 space_flag:dw 0
 image: times 21440 db 0   ;22L-6653       ;22L-6685
message1: db '22L-6653',0
message2: db '22L-6685',0
message3: db 'AMMAD ATIF',0
message4: db 'M.KAIF SARFRAZ',0
message5: db 'WELCOME',0
message6: db 'INSTRUCTIONS:',0
message7: db '1) PRESS ENTER TO START',0
message8: db '2) PRESS SPACE IN GAME TO JUMP ',0
message9: db '3) PRESS ESCAPE IN GAME TO EXIT',0
message10: db '4) GET CARROTS TO SCORE',0
message11: db '5) BLUE TILE BREAKS AFTER 3 SECOND',0
message12: db 'YOUR TOTAL SCORE IS: ',0
message13: db "","$"
message14: db 'DO YOU WANT TO EXIT',0
message15: db 'Y)YES          N)NO',0

s: dw 0
shiftCheck1: dw 0
shiftCheck2: dw 0
shiftCheck3: dw 0
rLoc: dw 59995   
score: dw 0
scoreString: db 'SCORE:','$'
escape_flag : db 0
blueTimer: dw 0
bluecheck: db 0
buff        db  25 dup(0), 10, 13
lbuff       EQU ($ - buff)              ; bytes in a string
musicArray: dw 2559,4304,4560,4831,5119,5423,6449,6833,7670,1612,1809,2152
pcb: times 2*16 dw 0 ; space for 32 PCBs
stack: times 2*32 dw 0 ; space for 32 512 byte stacks
nextpcb: dw 1 ; index of next free pcb
current: dw 0 ; index of current pcb

musicArrayIndex: dw 0

oldisr: dd 0
oldTMR : dd 0
confirm_flag:db 0
endGame: dw 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
initPCB:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push si
	
	mov bx,1
	mov cl,5
	shl bx,cl
	
	mov ax,[bp+6]
	mov [pcb+bx+18],ax
	
	mov ax,[bp+4]
	mov [pcb+bx+16],ax
	
	mov [pcb+bx+22],ds
	mov si,1
	
	mov cl,6
	shl si,cl
	
	add si,32*2+stack
	
	mov [pcb+bx+14],si  ;sp 
	
	mov word[pcb+bx+26],0x200
	
	mov word[pcb+bx+28],0
	
	pop si
	pop cx
	pop bx
	pop ax
	mov sp,bp
	pop bp
	ret 4
	





timer:
	push ds
	
	push bx

	push cs
	pop ds
	
	cmp byte[cs:escape_flag],1
	je timerCont
    cmp byte[cs:bluecheck],1
	jne resetBlueTimer
	add word[cs:blueTimer],1
	jmp timerCont
	
	resetBlueTimer:
		mov word[cs:blueTimer],0
timerCont:	
	mov bx,[current]
	
	shl bx,5
	
	mov [pcb+bx+0],ax
	mov [pcb+bx+4],cx
	mov [pcb+bx+6],dx
	mov [pcb+bx+8],si
	mov [pcb+bx+10],di
	mov [pcb+bx+12],bp
	mov [pcb+bx+24],es
	
	pop ax ; read original bx from stack
	mov [pcb+bx+2], ax ; save bx in current pcb
	pop ax ; read original ds from stack
	mov [pcb+bx+20], ax ; save ds in current pcb
	pop ax ; read original ip from stack
	mov [pcb+bx+16], ax ; save ip in current pcb
	pop ax ; read original cs from stack
	mov [pcb+bx+18], ax ; save cs in current pcb
	pop ax ; read original flags from stack
	mov [pcb+bx+26], ax ; save cs in current pcb
	mov [pcb+bx+22], ss ; save ss in current pcb
	mov [pcb+bx+14], sp ; save sp in current pcb
			
	mov bx, [pcb+bx+28] ; read next pcb of this pcb
	mov [current], bx ; update current to new pcb
	mov cl, 5
	shl bx, cl ; multiply by 32 for pcb start


	mov cx, [pcb+bx+4] ; read cx of new process
	mov dx, [pcb+bx+6] ; read dx of new process
	mov si, [pcb+bx+8] ; read si of new process
	mov di, [pcb+bx+10] ; read diof new process
	mov bp, [pcb+bx+12] ; read bp of new process
	mov es, [pcb+bx+24] ; read es of new process
	mov ss, [pcb+bx+22] ; read ss of new process
	mov sp, [pcb+bx+14] ; read sp of new process
	push word [pcb+bx+26] ; push flags of new process
	push word [pcb+bx+18] ; push cs of new process
	push word [pcb+bx+16] ; push ip of new process
	push word [pcb+bx+20] ; push ds of new process
	
	mov ax, [pcb+bx+0] ; read ax of new process
	mov bx, [pcb+bx+2] ; read bx of new process
	pop ds ; read ds of new process
	
	jmp far[cs:oldTMR]
			


delay:
	push cx
	mov cx,0xffff
	d_loop1:
		loop d_loop1
	mov cx,0xffff
	d_loop2:
		loop d_loop2		
	pop cx
	ret


tone:
	mov bp,sp
	mov al,182
	out 43h,al
	 call delay
	 call delay
	 call delay
musicLoop:
	mov bx,word[cs:musicArrayIndex]
	mov ax,[cs:musicArray+bx]
	add word[cs:musicArrayIndex],2
	cmp word[cs:musicArrayIndex],24
	jne continue
	mov word[cs:musicArrayIndex],0
	
	
continue:
	out 42h,al
	mov al,ah
	out 42h,al
	
	in al,61h
	or al,03
	out 61h,al
	call delay
	call delay
	call delay
	call delay
	
	
	in al,61h
	and al,0xfc
	out 61h,al
	
	jmp tone
	
	
hooktimer:
	push ax
	push es
		
	push 0x0000
	pop	es
			
	mov	ax,word[es:8*4]						; saving offset and segment of old timer handler
	mov	word[oldTMR], ax
			
	mov	ax, word[es:8*4+2]
	mov	word[oldTMR+2], ax
			
	cli
	mov	word[es:8*4],timer			; hooking our own timer
	mov	[es:8*4+2], cs			
	sti

	pop	es
	pop ax	
	
	ret
	
	
unHookTimer:
	push ax 
	push bx
	push es
	
	push 0x0000
	pop es
		
	mov ax,[oldTMR]
	mov bx,[oldTMR+2]
	
	cli
	mov word[es:8*4],ax
	mov word[es:8*4+2],bx
	sti
	
	pop es
	pop bx
	pop ax
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GotCarrot:
	push bp
	push ax
	push cx
	
	
	mov cx,3
	mov al,182
	out 43h,al
	mov ax,9121	
gotc1:
	out 42h,al
	mov al,ah
	out 42h,al
	
	in al,61h
	or al,03
	out 61h,al
	call delay
	call delay
	call delay
	
	in al,61h
	and al,0xfc
	out 61h,al
	
    mov bp,sp
	mov al,182
	out 43h,al
	dec cx
	jnz gotc1
	mov cx,3
	mov al,182
	out 43h,al
	mov ax,1355
gotc2:
out 42h,al
	mov al,ah
	out 42h,al
	
	in al,61h
	or al,03
	out 61h,al
	call delay
	call delay
	call delay
	
	in al,61h
	and al,0xfc
	out 61h,al
    mov bp,sp
	mov al,182
	out 43h,al
	dec cx
	jnz gotc2
	
pop cx
pop ax
pop bp
ret	 
	
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
 shift:  
	push bp
	mov bp,sp
 
shiftStart:		;mov ah,0x01
        ; int 0x16
        ; jz back2
		; mov ah,0
        ; int 0x16
		; cmp al,0x20
		;jne escape_check
        ;call scrolldown	
back5:
		
		 cmp word[space_flag],1
		 jne escape_check		
		 call scrolldown
		 cmp word[endGame],1
		 je shiftEnd
		 jmp back2
		  
	     escape_check:
		 mov ah,01h
		 int 16h
		 jz back2
		 mov ah,00h
		 int 16h
		 cmp al,0x1b
		 jne back2
		
				   
				   push ds
				   pop es
				   
		           push 0xa000
	               pop ds					; point es to video base
	               mov si, 21440
		           mov di,image
		           mov cx,21440
		           cld
		           rep movsb
				   
				   push es
				   pop ds
				   
		
		call confirm
		; cmp byte[confirm_flag],1              ;---------------------------------------------------sinidn4ndi4nd4
		cmp word[endGame],1
		jne restoreOGScreen
		jmp shiftEnd
		 
		 restoreOGScreen:
		        
				;push ds
				;push es
		        push 0xA000
	            pop es				; point es to video base
	            mov di, 21440	
		        mov si,image
		        ;mov ds,bx
		        ;mov si,0
		        mov cx,21440
		        cld
		        rep movsb
				
				;pop es
				;pop ds
		
back2: cmp byte[bluecheck],1
        je BreakCheck
back4:  call shiftSlab
        mov ax,20
       
shift1:	  push ax
	  call shiftRight
	  add ax,1
	  cmp ax,66
	  je shiftloop_l1
	  jmp shift1
shiftloop_l1: 
      mov ax,70
shift2:	  push ax
	  call shiftLeft
	  add ax,1
	  cmp ax,130
	  je shiftStart
	  jmp shift2

BreakCheck:
;add word[blueTimer],1
cmp word[blueTimer],2000
jl back4
mov word[endGame],1

shiftEnd:
	mov sp,bp
	pop bp
	ret
	
	
				
printNumTemp: push bp
				mov bp, sp
				push es
				push ax
				push bx
				push cx
				push dx
				push di

				mov ax, [bp+4]	; load number in ax= 4529
				mov bx, 10			; use base 10 for division
                mov cx, 0			; initialize count of digits

nextDigit:		mov dx, 0			; zero upper half of dividend
				div bx				; divide by 10 AX/BX --> Quotient --> AX, Remainder --> DX ..... 
				add dl, 0x30		; convert digit into ascii value
				push dx				; save ascii value on stack

				inc cx				; increment count of values
				cmp ax, 0			; is the quotient zero
				jnz nextDigit		; if no divide it again


				mov dl,[bp+8]
                mov dh,[bp+6]
                mov ah,02h
                int 10h
				
nextPos:		pop dx				; remove a digit from the stack
				MOV AH,02H
                INT 21H
				add di, 2			; move to next screen location
				loop nextPos		; repeat for all digits on stack

				pop di
				pop dx
				pop cx
				pop bx
				pop ax
				pop es
				mov sp,bp
				pop bp
				ret 6
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
printnum: push bp
				mov bp, sp
				push es
				push ax
				push bx
				push cx
				push dx
				push di

				mov ax, [score]		; load number in ax= 4529
				mov bx, 10			; use base 10 for division
                mov cx, 0			; initialize count of digits

nextdigit:		mov dx, 0			; zero upper half of dividend
				div bx				; divide by 10 AX/BX --> Quotient --> AX, Remainder --> DX ..... 
				add dl, 0x30		; convert digit into ascii value
				push dx				; save ascii value on stack

				inc cx				; increment count of values
				cmp ax, 0			; is the quotient zero
				jnz nextdigit		; if no divide it again


				mov dl,28
                mov dh,1
                mov ah,02h
                int 10h
                lea dx,scoreString
                mov ah,09h
                int 21h
nextpos:		pop dx				; remove a digit from the stack
				MOV AH,02H
                INT 21H
				add di, 2			; move to next screen location
				loop nextpos		; repeat for all digits on stack

				pop di
				pop dx
				pop cx
				pop bx
				pop ax
				pop es
				mov sp,bp
				pop bp
				ret 
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------				
randomNumGen:         ; generate a rand no using the system time
			push bp
			mov bp,sp
			sub sp,2
			push bx
			push cx
			push dx
			mov ax,[bp+4]  ;end    2
			mov bx,[bp+6]	;start 0
	
			sub ax,bx	;end-start
			mov word[bp-2],ax
			add word[bp-2],1
			
			
			;mov ah,00h  ; interrupts to get system time        
			;int 1Ah   ; cx:dx now hold number of clock ticks since midnight   
			rdtsc

			;mov  ax, dx
			xor  dx, dx
			mov  cx, word[bp-2]
			div  cx       ; here dx contains the remainder of the division - from end to start
			add dx,bx
			mov ax,dx
			
			pop dx
			pop cx
			pop bx
			add sp,2
			mov sp,bp
			pop bp
			ret 4
				
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
Display : 
      
      call clrscr	  
      mov ax,90
	  push ax               ; push size............[bp+8]
	  mov ax, 65
	  push ax				; push r position............[bp+6]
	  mov ax, 300
	  push ax				; push c position............[bp+4]
	  call printMountain
	  mov ax,90
	  push ax               ; push size............[bp+8]
	  mov ax, 66
	  push ax				; push r position............[bp+6]
	  mov ax, 40
	  push ax				; push c position............[bp+4]
	  call printMountain
	  mov ax,80
	  push ax               ; push size............[bp+8]
	  mov ax, 66
	  push ax				; push r position............[bp+6]
	  mov ax, 110
	  push ax				; push c position............[bp+4]
	  call printMountain
	  mov ax,70
	  push ax               ; push size............[bp+8]
	  mov ax, 66
	  push ax				; push r position............[bp+6]
	  mov ax, 150
	  push ax				; push c position............[bp+4]
	  call printMountain
	  mov ax,80
	  push ax               ; push size............[bp+8]
	  mov ax, 66
	  push ax				; push r position............[bp+6]
	  mov ax, 196
	  push ax				; push c position............[bp+4]
	  call printMountain
	  mov ax,80
	  push ax               ; push size............[bp+8]
	  mov ax, 66
	  push ax				; push r position............[bp+6]
	  mov ax, 240
	  push ax				; push c position............[bp+4]
	  call printMountain
	  mov ax,40
	  push ax
	  mov ax,90
	  push ax
	  mov ax,160
	  push ax
	  call printcar
	  call printLast
	  call Sun
	  call printnum
      ret
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------	  
printcar:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push si
	push di
	
	mov dx,0
    mov ax, 0xA000
	mov es, ax                   ; point es to video base
ship:
    
	add word[s],1
	mov ax, 320                  ; load al with columns per row
	mul word [bp + 6]            ; 320 x r
	add ax, [bp + 4]             ; word number (320xr) + c
	mov di, ax
	mov cx, [bp + 8]
	mov si, [bp + 8]
	
	cmp word[s],1
	jz a
	cmp word[s],2
	jz b
	cmp word[s],3
	jz c
	
	jmp fin
	
a:
nextc:
    mov byte [es:di], 0x0f         ; show this color on screen
	add di, 1                    ; move to next screen location
	dec cx
	cmp cx, 0
	je ab
	jmp nextc
	
ab: sub si, 2
	mov cx, si
	sub ax, 320
	mov di, ax
	cmp si, 0
	je ship
	jmp nextc
	
b:
nextc2:
    mov byte [es:di], 0x0f      ; show this color on screen
	sub di, 1                    ; move to next screen location
	dec cx
	cmp cx, 0
	je ab2
	jmp nextc2
	
ab2: sub si, 2
	mov cx, si
	sub ax, 320
	mov di, ax
	cmp si, 0
	je ship
	jmp nextc2

c:
push cx 
mov cx,10
stick: 
    mov byte [es:di], 0xff         ; show this color on screen
	add di, 320     ; move to next screen location
	add ax,320
	dec cx
	cmp cx, 0
	jne stick
	
    pop cx
	add cx,30
	add si,30
    push si
	push cx
	push di
	push ax
nextc3:
    mov byte [es:di], 0x06    ; show this color on screen
	sub di, 1                    ; move to next screen location
	dec cx
	cmp cx, 0
	je ab3
	jmp nextc3
	
ab3: sub si, 2
	mov cx, si
	add ax, 320
	mov di, ax
	cmp si, 20
	jne nextc3
	
	pop ax
	pop di
	pop cx
	pop si
nextc4:
    mov byte [es:di], 0x06    ; show this color on screen
	add di, 1                    ; move to next screen location
	dec cx
	cmp cx, 0
	je ab4
	jmp nextc4
	
ab4: sub si, 2
	mov cx, si
	add ax, 320
	mov di, ax
	cmp si, 20
	je ship
	jmp nextc4
fin:
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 6  
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
clrscr:		push es
			push ax
			push di

			mov ax, 0xA000
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column

nextloc:	mov byte [es:di], 100  	    ; clear next char on screen
			add di, 1					; move to next screen location
			cmp di, 21440				; has the whole screen cleared
			jne nextloc	
nextl:  	mov byte [es:di], 0x0B   	; clear next char on screen
			add di, 1					; move to next screen location
			cmp di, 42880				; has the whole screen cleared
			jne nextl
nextlo:  	mov byte [es:di], 0x15  	; clear next char on screen
			add di, 1					; move to next screen location
			cmp di, 64000				; has the whole screen cleared
			jne nextlo
			
			pop di
			pop ax
			pop es
			ret
				
cursor_off : 
mov     ah, 00h
int     16h
ret	
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
shiftSlab: push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds
			
			mov ax,0xA000
			mov es,ax
			
            mov ax,word[shiftCheck1]
		    cmp ax,0
			je sr1
			jne sl1
b30:		mov ax,word[shiftCheck2]
		    cmp ax,0
			je sr2
			jne sl2
b31:		mov ax,word[shiftCheck3]
		    cmp ax,0
			je sr3
			jne sl3
b1:         
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			mov sp,bp
			pop bp
			ret 			

sr1:
mov si,49170
mov al,[es:si]
cmp al,0x15
jne red1
mov si,49070	
b10:	mov al,[es:si]
	cmp al,47
	je b30
	cmp al,32
	je b30
	;cmp al,42
	;je b18
	add si,2
	cmp si,49170
	jne b10
b4:	mov ax,153
	push ax 
	call shiftRight
	mov ax,154
	push ax
	call shiftRight
	jmp b30
red1: mov ax,1
     mov [shiftCheck1],ax
     jmp b30	
	
sl1: mov si,49070
    mov al,[es:si]
	cmp al,0x15
	jne incr1
    mov si,49070
b11:	mov al,[es:si]
	cmp al,47
	je b30
	cmp al,32
	je b30
	add si,2
	cmp si,49170
	jne b11
b12:	mov ax,153
	push ax 
	call shiftLeft
	mov ax,154
	push ax
	call shiftLeft
	jmp b30
incr1: mov ax,0
     mov [shiftCheck1],ax
     jmp b30	

b24: 	;call delay
    jmp b12		
b18:
;call delay
    jmp b4	
sr2:
mov si,56210          ;63500
mov al,[es:si]
cmp al,0x15
jne red2
mov si,56110
b9:	mov al,[es:si]
	cmp al,47
	je b31
	cmp al,32
	je b31
	;cmp al,42
	;je b20
	add si,2
	cmp si,56210
	jne b9
b3:	mov ax,175	
	push ax
	call shiftRight
	mov ax,176
	push ax
	call shiftRight
	jmp b31
red2: mov ax,1
     mov [shiftCheck2],ax
     jmp b31
sl2:
	mov si,56110
    mov al,[es:si]
	cmp al,0x15
	jne incr2
	mov si,56110
b13:	mov al,[es:si]
	cmp al,47
	je b31
	cmp al,32
	je b31
	;cmp al,42
	;je b26
	add si,2
	cmp si,56210
	jne b13
b14:	mov ax,175	
	push ax
	call shiftLeft
	mov ax,176
	push ax
	call shiftLeft
	jmp b31	
incr2: mov ax,0
     mov [shiftCheck2],ax
     jmp b31	
b26: call delay	
    jmp b14
b20:
	call delay	
    jmp b3
sr3:
mov si,63570
mov al,[es:si]
cmp al,0x15
jne red3
mov si,63470
b8:	mov al,[es:si]
	cmp al,47
	je b1
	cmp al,32
	je b21
	;cmp al,42
	;je b22
	add si,2
	cmp si,63570
	jne b8
b19:
     add word[rLoc],1
	mov ax,178	
b7:	push ax
	call shiftRight
	add ax,1
	cmp ax,200
	jne b7
	jmp b1
red3: mov ax,1
     mov [shiftCheck3],ax
     jmp b1	
sl3:
	mov si,63470
	mov al,[es:si]
	cmp al,0x15
	jne incr3
	mov si,63470
b15:	mov al,[es:si]
	cmp al,47
	je b1
	cmp al,32
	je b21
	;cmp al,42	
	;je b28
	add si,2
	cmp si,63570
	jne b15
b17: 
	sub word[rLoc],1
	mov ax,178	
b16:	push ax
	call shiftLeft
	add ax,1
	cmp ax,200
	jne b16
	jmp b1
incr3: mov ax,0
     mov [shiftCheck3],ax
     jmp b1	
b28:   call delay
	jmp b17
b22:
call delay
	jmp b19
b21:
mov byte[bluecheck],1
jmp b1	
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
shiftRight:	push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds
          
			mov ax, [bp+4] ; load chars per row in ax
			mov cx,320
			mul cx
			add ax,319
			mov di, ax ; load source position in si
			mov ax, 0xA000
			mov es, ax ; point es to video base
			mov al,byte[es:di]
			push ax
			mov si,di
			sub si,1

			mov cx, 319 ; number of screen locations
			
l1:			mov al,[es:si]
			mov [es:di],al
			sub si,1
			sub di,1
			loop l1
			
			
			pop ax
			mov [es:di],al
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			mov sp,bp
			pop bp
			ret 2	
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
			
shiftLeft:	push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds
          
			mov ax, [bp+4] ; load chars per row in ax
			mov cx,320
			mul cx
			;add ax,319
			mov di, ax ; load source position in si
			mov ax, 0xA000
			mov es, ax ; point es to video base
			mov al,byte[es:di]
			push ax
			mov si,di
			add si,1

			mov cx, 319 ; number of screen locations
			
l2:			mov al,[es:si]
			mov [es:di],al
			add si,1
			add di,1
			loop l2
			
			
			pop ax
			mov [es:di],al
			
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			mov sp,bp
			pop bp
			ret 2	
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
scrolldown:	push bp
			mov bp,sp
			push ax
			push dx
			push bx
			push cx
			push si
			push di
			push es
			push ds
            
			mov cx,11
			mov ax, 320 ; load chars per row in ax
			mul cx      ; calculate source position
			
			mov si, 63999 ; last location on the screen
			sub si, ax ; load source position in si
			
			mov cx, 17600 ; number of screen locations
			mov ax, 0xA000
			mov es, ax ; point es to video base
			mov di,63999

l6:
            mov al,[es:si]
			mov [es:di],al
			sub si,1
			sub di,1
			;call delay
			loop l6			
			
			mov si,60480
			mov cx,3520

l11:         mov byte[es:si],0x15
            inc si
            dec cx
			jnz l11
			
			mov si,43200
			mov cx,3520
			
			mov byte[bluecheck],0
	        mov word[blueTimer],0
			
l3:         mov byte[es:si],0x15
            inc si
            dec cx
			jnz l3
			
			mov dl,47
			;sub dl,byte[bp+4]
			jmp choose
			
back1:			mov ax,1
			push ax
			mov ax,3
			push ax
			call randomNumGen
			cmp ax,1
			je go1
			cmp ax,2
			je go2
			jmp go3 
			
go1:		mov si,45900
            jmp back3
go2:		mov si,45895
            jmp back3
go3:        mov si,45905			
back3:		mov cx,30	
l4:         mov byte[es:si],dl
            inc si
            dec cx
			jnz l4
			sub si,350
			
			mov cx,30
l5:         mov byte[es:si],dl
            ;call delay
            inc si
            dec cx
			jnz l5

mov ax,1
push ax
mov ax,3
push ax
call randomNumGen
cmp ax,1
jne nex3
mov di,48785			
mov si,di           ;Carrots
mov cx,5
l8:
mov byte[es:si],4
sub si,319
dec cx
jnz l8
mov si,di
mov cx,5
l9:
mov byte[es:si],4
sub si,321
dec cx
jnz l9
mov si,di
sub si,1604
mov cx,9
l10:
mov byte[es:si],4
add si,1
dec cx
jnz l10
nex3:		sub word[rLoc],3520	
			call printRabbit
		    mov si,[rLoc]
    		add si,3522
			
			mov al,byte[es:si]   ;checks if game over
			cmp al,0x15
			jne e2
			add si,5
			mov al,byte[es:si]
			cmp al,0x15
			jne e2
			mov word[endGame],1
			jmp scrollDownEnd
			
e2:			mov si,[rLoc]
			sub si,2240
			mov al,byte[es:si]    ; check if it get Carrots
			cmp al,4
			je e4
			add si,9
			mov al,byte[es:si]
			cmp al,4
			jne e3
			
e4:			add word[score],100
            call printnum
			call GotCarrot
e3:			call delay
            call delay
			call delay
			call delay
            call delay
			call delay
			call delay
            call delay
			call delay
			call delay
            call delay
            mov cx,11
			mov ax, 320 ; load chars per row in ax
			mul cx      ; calculate source position
			
			mov si, 63999 ; last location on the screen
			sub si, ax ; load source position in si
			
			mov cx, 17600 ; number of screen locations
			mov di,63999

l12:
            mov al,[es:si]
			mov [es:di],al
			sub si,1
			sub di,1
			loop l12
            mov si,43200
			mov cx,3520
l13:         mov byte[es:si],0x15
            inc si
            dec cx
			jnz l13
			mov si,56640
			mov cx,3520
l14:         mov byte[es:si],0x15
            inc si
            dec cx
			jnz l14
			add word[rLoc],3520
			call printRabbit
			
			mov word[space_flag],0
			
			
		scrollDownEnd:
            pop ds
			pop es
			pop di
			pop si
			pop cx
			pop bx
			pop dx
			pop ax
			mov sp,bp
			pop bp
			ret 

choose:
mov ax,1
push ax
mov ax,4
push ax
call randomNumGen     
cmp ax,1
jne nex1
sub dl,5
jmp back1
nex1:
cmp ax,2
jne nex2
sub dl,3
jmp back1
nex2:
cmp ax,3
jne back1
sub dl,15
jmp back1
		
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------

printLast:	push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push si
			push di
           
			mov ax, 0xA000
			mov es, ax				; point es to video base

			mov si,56460
			mov cx,30
n18:	
			mov byte [es:si], 47			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n18
			mov si,56140
			mov cx,30
n19:	
			mov byte [es:si], 47			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n19
			mov si,49430
			mov cx,30
n20:	
			mov byte [es:si], 44			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n20
			mov si,49110
			mov cx,30
n21:	
			mov byte [es:si], 44			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n21
			mov si,63185
			mov cx,30
n22:	
			mov byte [es:si], 47			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n22
			mov si,63505
			mov cx,30
n23:	
			mov byte [es:si], 47			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n23
			
            ;mov ax,[rLoc]
			;push ax
			
			call printRabbit
			
            pop di
			pop si
			pop cx
			pop bx
			pop ax
			pop es
			pop bp
			ret 
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
printRabbit:	push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push si
			push di	
			
            mov si,word[rLoc]
			mov cx,10
			mov ax, 0xA000
			mov es, ax			
n2:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n2
            sub si,327
			mov cx,4
n3:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n3			
            sub si,325           ;59037
			mov cx,6
n4:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n4
			sub si,327    ;58716
			mov cx,8
n5:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n5
			sub si,327    ;58397
			mov cx,6
n6:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n6
			sub si,325   ;58078
			mov cx,4
n7:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n7
			sub si,325     ;57757
			mov cx,3
n8:	
			mov byte [es:si], 15			; show this color on screen
			sub si,321			;  move to next screen location		
			dec cx
			jnz n8
			add si,968    ;57762
			mov cx,3
n9:	
			mov byte [es:si], 15			; show this color on screen
			sub si,319			;  move to next screen location		
			dec cx
			jnz n9
			add si,3191  ;59996  
			mov cx,8
n10:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n10
			add si,312   ;60316
			mov cx,8
n11:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n11
			add si,313    ;60637
			mov cx,6
n12:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n12
			add si,314    ;60957
			mov cx,6
n13:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n13
			add si,314     ;61277
			mov cx,6
n14:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n14
			add si,315    ;61598
			mov cx,4
n15:	
			mov byte [es:si], 15			; show this color on screen
			add si,1			;  move to next screen location		
			dec cx
			jnz n15
		     ;61602
			mov cx,4
n16:	
			mov byte [es:si], 15			; show this color on screen
			add si,320			;  move to next screen location		
			dec cx
			jnz n16
			sub si,1285
			mov cx,4
n17:	
			mov byte [es:si], 15			; show this color on screen
			add si,320			;  move to next screen location		
			dec cx
			jnz n17
			
		    pop di
			pop si
			pop cx
			pop bx
			pop ax
			pop es
			mov sp,bp
			pop bp
			
			ret 
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------

printMountain:	push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push si
			push di

ag:			mov ax, 0xA000
			mov es, ax				; point es to video base

			mov ax, 320				; load al with columns per row
			mul word [bp+6]		    ; 320 x r
			add ax, [bp+4]			; word number (320xr) + c
			mov di,ax
            mov cx,[bp+8]
			mov si,[bp+8]
nextchar:	
			mov byte [es:di], 30			; show this color on screen
			add di,1			            ;  move to next screen location		
			dec cx
			cmp cx,0
			je up
			jmp nextchar

back:       
            pop di
			pop si
			pop cx
			pop bx
			pop ax
			pop es
			pop bp
			ret 6
			
up:  sub si,2
     mov cx,si
	 sub ax,319
	 mov di,ax
     cmp si,0
	 je back
	 jmp nextchar
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------

Sun:    	push bp
			mov bp, sp
			push es
			push ax
			push bx
			push cx
			push si
			push di

    		mov ax, 0xA000
			mov es, ax				; point es to video base
            
			mov cx,1
			mov ax, 320				; load al with columns per row
			mul cx       		    ; 320 x r
			add ax,160 			; word number (320xr) + c
			mov di,ax
			mov cx,3
nextch1:	
			mov byte [es:di], 42			; show this color on screen
			add di,320			            ;  move to next screen location		
			dec cx
			jnz nextch1
			add di,320
			mov cx,1
			mov si,3
			mov bx,2
nextch2:	
			mov byte [es:di], 42			; show this color on screen
			add di,1			            ;  move to next screen location		
			dec cx
			cmp cx,0
			je down
            jne nextch2		
n:          add di,2
			mov cx,3
			mov si,5
			mov bx,2
			
nextch3:	
			mov byte [es:di], 42			; show this color on screen
			add di,1			            ;  move to next screen location		
			dec cx
			cmp cx,0
			je up1
            jne nextch3	 

n1:         mov cx,7
			mov ax, 320				; load al with columns per row
			mul cx       		    ; 320 x r
			add ax,164 			; word number (320xr) + c
			mov di,ax
			mov cx,3
nextch4:	
			mov byte [es:di], 42			; show this color on screen
			add di,1			            ;  move to next screen location		
			dec cx
			jnz nextch4	

            mov cx,11
			mov ax, 320				; load al with columns per row
			mul cx       		    ; 320 x r
			add ax,160 			; word number (320xr) + c
			mov di,ax
			mov cx,3
nextch5:	
			mov byte [es:di], 42			; show this color on screen
			add di,320			            ;  move to next screen location		
			dec cx
			jnz nextch5	

            mov cx,7
			mov ax, 320				; load al with columns per row
			mul cx       		    ; 320 x r
			add ax,156 			; word number (320xr) + c
			mov di,ax
			mov cx,3
nextch6:	
			mov byte [es:di], 42			; show this color on screen
			sub di,1			            ;  move to next screen location		
			dec cx
			jnz nextch6	 			
			
       
            pop di
			pop si
			pop cx
			pop bx
			pop ax
			pop es
			pop bp
			ret 

down:  mov cx,si
	   add si,2
       add di,320
	   sub di,bx
	   add bx,2
	   cmp si,9
	   je n
	   jmp nextch2
up1:  mov cx,1
	   sub si,2
       add di,320
	   sub di,bx
	   sub bx,2
	   cmp si,1
	   je n1
	   jmp nextch3



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

start_messages:
 mov ah, 2         ; Function 2 - Set cursor position
      mov bh, 0         ; Page number (0 for mode 13h)
      mov dh, 2      ; Row (0-based)
    mov dl,5  ; Column (0-based)
    int 0x10
	
    lea si, [message1] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next1_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz next2           ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next1_msg
	
next2:
	mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 2      ; Row (0-based)
    mov dl, 25  ; Column (0-based)
    int 0x10
	
    lea si, [message2] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next2_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next3   ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next2_msg
	
next3:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 4     ; Row (0-based)
    mov dl, 5  ; Column (0-based)
    int 0x10
	
    lea si, [message3] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next3_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next4  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next3_msg
	
	next4:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 4     ; Row (0-based)
    mov dl, 25 ; Column (0-based)
    int 0x10
	
    lea si, [message4] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next4_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next5  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next4_msg
	
next5:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 7   ; Row (0-based)
    mov dl, 15 ; Column (0-based)
    int 0x10
	
    lea si, [message5] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next5_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next6  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next5_msg
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	next6:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 10  ; Row (0-based)
    mov dl, 5 ; Column (0-based)
    int 0x10
	
    lea si, [message6] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next6_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next7  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next6_msg

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	next7:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 12   ; Row (0-based)
    mov dl, 5 ; Column (0-based)
    int 0x10
	
    lea si, [message7] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next7_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next8 ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next7_msg
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	next8:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 14   ; Row (0-based)
    mov dl, 5 ; Column (0-based)
    int 0x10
	
    lea si, [message8] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next8_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next9 ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next8_msg
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	next9:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 16   ; Row (0-based)
    mov dl, 5 ; Column (0-based)
    int 0x10
	
    lea si, [message9] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next9_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next10  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next9_msg
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	next10:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 18  ; Row (0-based)
    mov dl, 5 ; Column (0-based)
    int 0x10
	
    lea si, [message10] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next10_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    next11  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next10_msg
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	next11:
    mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 20  ; Row (0-based)
    mov dl, 5 ; Column (0-based)
    int 0x10
	
    lea si, [message11] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next11_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    done  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next11_msg
	
done:
     mov ah,0x01
     int 0x16
     jz done
     mov ah,0
     int 0x16
     cmp al,0x0d
     jne done
     ret 
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;--------------------------------------------------------------------
print_start_screen:		
            mov ax,13h
            int 10h
            push es
			push ax
			push di

			mov ax, 0xA000
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column
loop_print_start_screen:  	
            mov byte [es:di], 0xff  	; clear next char on screen
			add di, 1					; move to next screen location
			cmp di, 64000				; has the whole screen cleared
			jne loop_print_start_screen
			
			call start_messages
			pop di
			pop ax
			pop es
			ret
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;-------------------------------------------------------------------
unHookKeyBoard:
	push ax
	push bx
	push es
	
	push 0x0000
	pop es
	
	mov ax,word[oldisr]
	mov bx,word[oldisr+2]
	
	cli
	mov word[es:9*4],ax
	mov word[es:9*4+2],bx
	sti
	
	pop es
	pop bx
	pop ax
	ret
	
	

print_game_screen:
            push ax
			mov ax,13h
            int 10h
			call Display
            call shift
		  
            ;call cursor_off
		
			; mov ax, 0x4c00 ; terminate program
            ; int 0x21	
		printgameScreenEnd:
			pop ax
			ret
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;-------------------------------------------------------------------
 print_exit_screen:
            
			push ax
            push es

beforeExit:
	;------------------------------
	;   For disabling speaker before exit
			in al,61h
			and al,0xfc
			out 61h,al
	;-------------------------------
			
			mov ax, 0xA000
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column
loop_print_exit_screen:  	
            mov byte [es:di], 0xff  	; clear next char on screen
			add di, 1					; move to next screen location
			cmp di, 64000				; has the whole screen cleared
			jne loop_print_exit_screen
			
			call exit_messages
			pop es
			pop ax
			ret
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;-------------------------------------------------------------------
exit_messages:
	mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 12      ; Row (0-based)
    mov dl,5  ; Column (0-based)
    int 0x10
	
    lea si, [message12] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next1b_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz next2b          ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next1b_msg
	
next2b:
	call printscore
   
doneb:
     ret 
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;-------------------------------------------------------------------
confirm:
				push bp
				mov bp,sp
              push ax
			  push di
			  push es
			  push ds
			 
			  mov ax, 0xA000
			  mov es, ax					; point es to video base
			  mov di, 21440				; point di to top left column
  loop_print_confirm_screen:  	
              mov byte [es:di], 0xff  	; clear next char on screen
			  add di, 1					; move to next screen location
			  cmp di, 42880				; has the whole screen cleared
			  jne loop_print_confirm_screen

			call confirm_messages
			
			 pop ds
			 pop es
			 pop di
			 pop ax	 
			 mov sp,bp
			 pop bp
		     ret

;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;-------------------------------------------------------------------
confirm_messages:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	
	
      mov ah, 2         ; Function 2 - Set cursor position
      mov bh, 0         ; Page number (0 for mode 13h)
      mov dh, 10      ; Row (0-based)
    mov dl,10 ; Column (0-based)
    int 0x10
	
    lea si, [message14] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next1c_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz next2c          ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next1c_msg
	
next2c:
	mov ah, 2         ; Function 2 - Set cursor position
    mov bh, 0         ; Page number (0 for mode 13h)
    mov dh, 12   ; Row (0-based)
    mov dl, 10  ; Column (0-based)
    int 0x10
	
    lea si, [message15] ; Load the address of the message
    mov ah, 0x0E      ; Function 0x0E - Teletype output
    mov bh, 0         ; Page number (0 for mode 13h)
    mov bl, 7        ; Color palette index (e.g., white)
   
next2c_msg:
    lodsb             ; Load the next character from the message
    test al, al       ; Check if it's the null terminator
    jz    donec  ; If null terminator, exit
    int 0x10          ; Print the character
    jmp next2c_msg
	
donec:
     mov ah,0x01
     int 0x16
     jz donec
     mov ah,00h
     int 0x16
	 
	 cmp al,0x6e
	 je NoExit
	 cmp al,0x79
	 je ExitConfirmed
	 jmp donec
	 
     ; cmp al,0x79		;;  y key
     ; jne no
	 ; mov byte[confirm_flag],1
	 ; mov word[endGame],1
	 ; jmp now
	 ; no:
		; cmp al,0x6e		;;   n  keyy
		; jne donec
		; mov byte[confirm_flag],0
		
	ExitConfirmed:
		mov byte[confirm_flag],1
		mov word[endGame],1
		jmp now
		
	NoExit:
		mov byte[confirm_flag],0
		mov word[endGame],0
	
	 now:
			pop di
			pop si
			pop dx
			pop cx
			pop bx
			pop ax
			mov sp,bp
			pop bp
			ret 
;--------------------------------------------------------------------
; subroutine to print Mountains
; takes x position, y position, string attribute, address of string
; and its length as parameters
;-------------------------------------------------------------------
printscore: 	push bp
				mov bp, sp
				
				push ax
				push bx
				push cx
				push dx
				

				mov ax, [score]		; load number in ax= 4529
				mov bx, 10			; use base 10 for division
                mov cx, 0			; initialize count of digits

nextdigit1:		mov dx, 0			; zero upper half of dividend
				div bx				; divide by 10 AX/BX --> Quotient --> AX, Remainder --> DX ..... 
				add dl, 0x30		; convert digit into ascii value
				push dx				; save ascii value on stack

				inc cx				; increment count of values
				cmp ax, 0			; is the quotient zero
				jnz nextdigit1		; if no divide it again


				mov dl,25
                mov dh,12
                mov ah,02h
                int 10h
                lea dx,message13
                mov ah,09h
                int 21h
				
nextpos1:		pop dx				; remove a digit from the stack
				mov ah,02h
                int 21h
				add di,2			; move to next screen location
				loop nextpos1		; repeat for all digits on stack

				pop dx
				pop cx
				pop bx
				pop ax
				mov sp,bp
				pop bp
				ret 
				
kbisr:		push ax
			in al, 0x60						; read a char from keyboard port, scancode
	
			cmp al, 0x39					; is the key left shift
			jne nomatch				; no, try next comparison
			mov word[cs:space_flag],1
nomatch:		

			pop ax
			jmp far [cs:oldisr]	
			;iret

			
hookKeyBoard:
	push ax
	push es
	push 0x0000
	pop es
	
	mov ax,word[es:9*4]
	mov word[oldisr],ax
	
	mov ax,word[es:9*4+2]
	mov word[oldisr+2],ax
	
	cli
	mov word[es:9*4],kbisr
	mov word[es:9*4+2],cs
	sti
	
	pop es
	pop ax
	ret
	
		
start_screen:
		call print_start_screen; es=0, point es to IVT base

		mov byte[current],0
		mov word[pcb+28],1
		push cs
		push tone
		call initPCB
		call hooktimer	
		call hookKeyBoard					; enable interrupts 	
		mov ax, 1100
		out 0x40, al
		mov al, ah
		out 0x40, al
  
    call print_game_screen
   

terminate:

 call unHookTimer
 call unHookKeyBoard
 call print_exit_screen
 ;call cursor_off
 LastKey:
 mov ah,01h
 int 16h
 jz LastKey
 mov ah,00h
 int 16h
 cmp al,0xD
 jnz LastKey
 mov ax,0003h
 int 10h
 mov ax, 0x4c00 ; terminate program
 int 0x21
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
 
