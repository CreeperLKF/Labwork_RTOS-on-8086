ifndef __RTOS8086_INC
__RTOS8086_INC equ 1
;; -------------------------------------
;   RTOS Structs and Definitions
;; -------------------------------------
; eTaskState struct
    eRunning = 0  ; A task is querying the state of itself, so must be running.
    eReady = 1    ; The task being queried is in a ready or pending ready list.
    eBlocked = 2  ; The task being queried is in the Blocked state.
    eSuspended = 3; The task being queried is in the Suspended state, or is in the Blocked state with an infinite time out.
    eDeleted = 4  ; The task being queried has been deleted, but its TCB has not yet been freed.
    eInvalid = 5  ; Used as an 'invalid state' value.
; eTaskState ends

portTASK_STACK_SIZE equ 100h
portRTOS_STACK_SIZE equ 100h

RTOS_FUNC_TAB struct
    fip dw ?
    fcs dw ?
RTOS_FUNC_TAB ends

RTOS_TASK_TCB struct
    func RTOS_FUNC_TAB <>
    lst_exec dw 0
    task_pri db 0
    task_stat db 0 ; LSB 0-2 bit for eTaskState, LSB 3 bit for timer enable (by default)
    task_timer dw 0
    task_stack db portTASK_STACK_SIZE dup (0)
    task_stack_bottom dw 0
    task_stack_top dw 0
RTOS_TASK_TCB ends

RTOS_IDLE_TAB equ RTOS_FUNC_TAB
RTOS_FUNC_TYPE equ <far>
portMAX_DELAY equ 0ffffh
;; -------------------------------------
;   RTOS Exported Symbols
;; -------------------------------------

ifdef __RTOS8086_ASM
    def_sth macro target
        public target
    endm

    def_near macro func
        public func    
    endm
    def_far macro func
        public func    
    endm
    req_far macro func
        extern func:far    
    endm
    req_near macro func
        extern func:near    
    endm
else
    def_sth macro target
        extern target
    endm

    def_far macro func
        extern func:far    
    endm
    def_near macro func
        extern func:near    
    endm
    req_near macro func
        public func    
    endm
    req_far macro func
        public func    
    endm
endif

__def_rtos_func macro func_type, func
    def_&func_type func
endm

def_rtos_func macro func
    __def_rtos_func % RTOS_FUNC_TYPE, func
endm

taskENTER_CRITICAL macro
    sti
endm

taskEXIT_CRITICAL macro
    cli
endm

def_near start_rtos
def_near stop_rtos
def_near exit_program
def_near add_table
def_near add_task_table
def_near add_idle_task_table
def_near del_table
def_near del_task_table
def_near del_idle_task_table

def_rtos_func vTaskSuspend
def_rtos_func vTaskResume
def_rtos_func vTaskDelete
def_rtos_func vTaskDelay
def_rtos_func vTaskDelayUntil

def_near get_keyboard_signal
def_near set_keyboard_signal
def_near get_task_current
def_near set_task_current
def_near main

req_near app_main
req_near pre_main_setup
req_near post_main_setup

endif