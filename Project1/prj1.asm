.model small
;; -------------------------------------
;   RTOS Data
;; -------------------------------------
RTOS_FUNC_TAB struct
    fip dw ?
    fcs dw ?
RTOS_FUNC_TAB ends

RTOS_TASK_TAB struct
    func RTOS_FUNC_TAB <>
    lst_exec dw 0
    task_pri db 0
    task_stat db 0
RTOS_TASK_TAB ends

RTOS_FUNC_TYPE equ <far>

.stack 100h
.data
    org 100h

    intt_1ch_addr dw 0, 0
    
    ; ticks serves as ticktime, ticks could be used to determine the loop time 
    ticks dw 0 ; the current ticks
    ticks_lst dw 0 ; update before task execute by ticks
    ticks_lst_lst dw 0 ; update after task execute by ticks_lst

    ; task_table from LSB to MSB: 2 IP, 2 CS, 2 lst_execute, 1 pri (ff is highest), 1 task status
    ; pri with 0 and pri with ff should be reserved
    task_table RTOS_TASK_TAB 3 dup (<>)
    task_table_idx_end dw 0
    soft_exit db 0

    idle_task_table RTOS_FUNC_TAB 3 dup (<>)
    idle_task_table_idx_end db 0
.const
;; -------------------------------------
;   User Data
;; -------------------------------------
.data
    digits db 8 dup (0)
    selected db 0
.const
    PORT_8255 equ 288h
    PORT_8255_A equ PORT_8255+0
    PORT_8255_B equ PORT_8255+1
    PORT_8255_C equ PORT_8255+2
    PORT_8255_CTRL equ PORT_8255+3
    DIGIT_2_LED db 0C0H, 0F9H, 0A4H, 0B0H, 99H, 92H, 82H, 0F8H, 80H, 90H
.code
;; -------------------------------------
;   RTOS Code - Internal
;; -------------------------------------

ticks_intt proc
    push ds
    mov ax, @data
    mov ds, ax
    inc word ptr [ticks]
    pop ds

    sti
    iret
ticks_intt endp

ticks_intt_setup proc near
    push es

    mov ax, 0
    mov es, ax
    ; backup interrupt address
    mov bx, word ptr es:[1ch*4]
    mov dx, word ptr es:[1ch*4+2]
    mov word ptr [intt_1ch_addr], bx
    mov word ptr [intt_1ch_addr+2], dx
    ; replace 1ch interrupt
    mov word ptr es:[1ch*4], @code
    mov word ptr es:[1ch*4+2], offset ticks_intt
    sti ; enable interrupt
    pop es
    ret
ticks_intt_setup endp

ticks_intt_restore proc near
    push es

    mov ax, 0
    mov es, ax
    ; restore 1ch interrupt
    mov bx, word ptr [intt_1ch_addr]
    mov dx, word ptr [intt_1ch_addr+2]
    mov word ptr es:[1ch*4], bx
    mov word ptr es:[1ch*4+2], dx

    pop es
    ret
ticks_intt_restore endp

scheduler_routine proc near
    ; Cooperative Scheduling, Non-preemptive
    sche_main_loop:
        idle_task_loop: ; loop until the ticks increases
            mov ax, word ptr [ticks]
            cmp ax, word ptr [ticks_lst]
            jne idle_task_loop_end

            xor si, si
            idle_task_func_loop: 
                cmp si, word ptr [idle_task_func_loop_end]
                je idle_task_func_loop_end
                
                call dword ptr idle_task_func_loop[si]
                add si, size RTOS_FUNC_TAB
            idle_task_func_loop_end: 
                nop
        idle_task_loop_end:
            nop
        
        inc ax
        cmp ax, word ptr [ticks_lst]
        je sche_normal_loop
        sche_slow_loop:
            ; show warning, a loop is too long
            ; this should not be triggered when Preemptive Scheduling is implemented
            nop
        sche_normal_loop:
            ; update ticks_lst
            mov word ptr [ticks_lst], ax

            ; find min task_pri or min lst_exec
            xor si, si ; current
            xor di, di ; highest target
            mov bx, word ptr task_table[si].lst_exec ; farest last execute time
            mov dx, word ptr task_table[si].task_pri ; highest function priority value
            add si, size RTOS_TASK_TAB

            sche_cmp_loop: ; two-dimensional partial order (highest task_pri, lowest lst_exec)
                cmp si, task_table_idx_end
                je sche_cmp_loop_end
                cmp word ptr task_table[si].task_pri, dx
                jb sche_cmp_loop_nxt
                ja sche_cmp_loop_swp
                cmp word ptr task_table[si].lst_exec, bx
                ja sche_cmp_loop_nxt

                sche_cmp_loop_swp: ; swap when find better
                    mov di, si
                    mov bx, word ptr task_table[si].lst_exec
                    mov dx, word ptr task_table[si].task_pri
                sche_cmp_loop_nxt: ; next
                    add si, size RTOS_TASK_TAB
                    jmp sche_cmp_loop
            sche_cmp_loop_end:
                nop
            ; execute target function
            mov word ptr task_table[di].lst_exec, ax
            call dword ptr task_table[di]
            ; update ticks_lst_lst
            mov ax, word ptr [ticks_lst]
            mov word ptr [ticks_lst_lst], ax

            cmp [soft_exit], 0
            jnz sche_main_loop

    ret
scheduler_routine endp

main_setup proc RTOS_FUNC_TYPE
    lea cx, main_task
    mov dl, 0fh
    call add_task_table
    call cli_setup

    ret
main_setup endp

main_task proc RTOS_FUNC_TYPE
    call app_main

    lea cx, main_task
    call del_task_table

    ret
main_task endp

main proc far
    mov ax, @data
    mov ds, ax
    mov es, ax
    mov ss, ax

    call start_rtos
    call stop_rtos ; will go here when soft_exit is not zero
    
    ret
main endp

;; -------------------------------------
;   RTOS Code - Exported
;; -------------------------------------

start_rtos proc near
    call main_setup

    call ticks_intt_setup

    call scheduler_routine

    ret
start_rtos endp

stop_rtos proc near
    call exit_program

    ret
stop_rtos endp

exit_program proc near
    call ticks_intt_restore
    mov ah, 4ch
    int 21h

    ret
exit_program endp

; si is &table[0], di is &table_idx_end, ax is the elem size
; cx is the function offset, dl is the priority
add_table proc near
    mov bx, word ptr [di]
    add word ptr [di], ax
    mov word ptr [si + bx].func.fip, cx
    mov ax, @code
    mov word ptr [si + bx].func.fcs, ax
    mov byte ptr [si + bx].task_pri, dl

    ret
add_table endp

add_task_table proc near ; cx is the function offset, dl is the priority
    lea si, task_table
    lea di, task_table_idx_end
    mov ax, size RTOS_TASK_TAB
    call add_table

    ret
add_task_table endp

add_idle_task_table proc near ; cx is the function offset
    lea si, idle_task_table
    lea di, idle_task_table_idx_end
    mov ax, size RTOS_FUNC_TAB
    call add_table

    ret
add_idle_task_table endp

; si is &table[0], di is &table_idx_end, ax is the elem size
; cx is the function offset
del_table proc near
    del_table_loop:
        cmp si, [di]
        je del_table_loop_end
        cmp word ptr [si].func.fip, cx
        jne del_table_loop_nxt

        mov bx, di
        sub [bx], ax
        mov di, si
        add di, ax
        xchg si, di
        cld
        mov_tab_loop:
            cmp di, [bx]
            je del_table_loop_end

            lodsb
            stosb
            jmp mov_tab_loop
        mov_tab_loop_end:
            nop

        jmp del_table_loop_end
        del_table_loop_nxt:
            add si, ax
            jmp del_table_loop
    del_table_loop_end:
        nop

    ret
del_table endp

del_task_table proc near ; cx is the function offset
    lea si, task_table
    lea di, task_table_idx_end
    mov ax, size RTOS_TASK_TAB
    call del_table

    ret
del_task_table endp

del_idle_task_table proc near ; cx is the function offset
    lea si, idle_task_table
    lea di, idle_task_table_idx_end
    mov ax, size RTOS_TASK_TAB
    call del_table

    ret
del_idle_task_table endp

cli_setup proc near
    lea cx, cli_task
    call add_idle_task_table

    ret
cli_setup endp

cli_task proc RTOS_FUNC_TYPE
    ; mov dl, byte ptr '-'
    ; mov ah, 2h
    ; int 21h

    ; detect if the user has input q
    mov ah, 0bh
    int 21h
    test al, al
    jnz if_detect_input_else

    if_detect_input:
        mov ah, 1h
        int 21h
        cmp al, byte ptr 'q'
        jne if_detect_input_else
        call exit_program
    if_detect_input_else:
        nop

    ret
cli_task endp

;; -------------------------------------
;   User Code
;; -------------------------------------

x8255A_setup proc near
    mov dx, PORT_8255_CTRL
    ; mode 0, port AB output, port Cl output, port Cu input
    mov al, 10001000b
    out dx, al

    ret
x8255A_setup endp

keyboard_setup proc near
    lea cx, keyboard_task
    mov dl, 0feh
    call add_task_table

    ret
keyboard_setup endp

keyboard_task proc RTOS_FUNC_TYPE
    mov dx, PORT_8255_C
    if_input_detected:
        xor bx, bx
        mov ah, 11110111b

        mov cx, 4
        detect_row_loop: ; detect row
            mov al, ah
            shr ah, 1
            and al, 0fh ; get masked scan code input
            
            out dx, al
            in al, dx ; get scan code output

            add bl, 4 ; point to the next column
            and al, 0f0h
            cmp al, 0f0h
            loopne detect_row_loop

        cmp al, 0f0h ; if no input is detected
        je if_no_input

        mov cx, 4
        detect_col_loop: ; detect col
            dec bl
            test al, 80h ; the higher position of 0 in scan code, the larger number 
            jnz if_bit4_is_0_not

            if_bit4_is_0:
                jmp if_do_input
            if_bit4_is_0_not:
                nop

            shl al, 1
            loop detect_row_loop

        jmp if_no_input
    if_input_detected_else:
        nop

    if_do_input:
        call keyboard_pressed
    if_no_input:
        nop

    ret
keyboard_task endp

keyboard_pressed proc near ; use bl to pass pressed key
    cmp bl, 0ah
    jb to_output_digit

    to_select_digit: ; a-f select which digit for update
        sub bl, 0ah
        mov [selected], bl
        jmp keyboard_pressed_ret
    to_output_digit: ; 0-9 tells what dight for display
        mov dl, bl
        mov bx, word ptr [selected]
        mov digits[bx], dl
    keyboard_pressed_ret:
        nop
    ret
keyboard_pressed endp

display_setup proc near
    lea cx, display_task
    call add_idle_task_table

    ret
display_setup endp

display_task proc RTOS_FUNC_TYPE
    mov cx, 8
    mov ah, 11111110b
    display_loop:
        mov al, cl
        dec al
        mov dl, al
        lea bx, digits
        xlatb ; which digit
        lea bx, DIGIT_2_LED
        xlatb ; what to display
        cmp dl, [selected] ; if the current is selected
        jne if_cur_is_selected_else
        if_cur_is_selected:
            and al, 7h ; add the dot
        if_cur_is_selected_else:
            nop
        mov dx, PORT_8255_A
        out dx, al
        
        mov al, ah
        mov dx, PORT_8255_B
        out dx, al
        
        ror ah, 1
        loop display_loop

    ret
display_task endp

app_main proc near
    call x8255A_setup
    call display_setup
    call keyboard_setup

    ret
app_main endp

end main