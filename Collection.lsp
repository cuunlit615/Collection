;; TB_Integrated_Pro.lsp — 標籤自動遞增 (含模式選擇與記憶功能)
;; -------------------------------------------------------------------

(vl-load-com)

;; --- 全域變數預設 TB ---
;; --- 全域變數預設 (支援 TB_SET 修改與工作階段記憶) ---
(if (not *TBlinc_mode*)   (setq *TBlinc_mode* "CL"))     ; 預設模式 CL
(if (not *TBlinc_prefix*) (setq *TBlinc_prefix* "A"))    ; 預設前綴
(if (not *TBlinc_start*)  (setq *TBlinc_start* 1))       ; 預設起始號
(if (not *TBlinc_tag*)    (setq *TBlinc_tag* "M00"))     ; 預設標籤
(if (not *TBlinc_coltol*) (setq *TBlinc_coltol* 1.5))   ; 預設容差 1.5

;; --- 內部工具函式 ---
(defun _zpad2 (n / s) ; 固定為 2 位數補零
  (setq s (itoa n))
  (if (= (strlen s) 1) (strcat "0" s) s)
)

(defun _pt-of (e / ed typ p)
  (setq ed (entget e) typ (cdr (assoc 0 ed)))
  (cond ((member typ '("TEXT" "MTEXT" "ATTRIB" "INSERT"))
         (setq p (cdr (assoc 10 ed)))))
  (if p p (list 0.0 0.0 0.0))
)

;; 支援 MText 屬性與標準文字更新
(defun _update-TB-vla (e new tag / obj name done)
  (setq obj (vlax-ename->vla-object e)
        name (vla-get-ObjectName obj))
  (cond
    ((member name '("AcDbText" "AcDbMText" "AcDbAttributeReference"))
     (vla-put-TextString obj new) (vla-update obj) T)
    ((= name "AcDbBlockReference")
     (if (= :vlax-true (vla-get-HasAttributes obj))
       (progn
         (foreach a (vlax-invoke obj 'GetAttributes)
           (if (= (strcase (vla-get-TagString a)) (strcase tag))
               (progn (vla-put-TextString a new) (setq done T))
           ))
         (vla-update obj) done))))
)

;; 欄群組化排序 (CL 邏輯)
(defun _sort-CL (lst / xs tolX sorted buckets curX cur items result)
  (setq tolX *TBlinc_coltol*)
  (setq sorted (vl-sort lst (function (lambda (a b) (< (car (cdr a)) (car (cdr b)))))))
  (setq buckets '() curX nil cur '())
  (foreach it sorted
    (setq items (cdr it))
    (if (or (null curX) (> (abs (- (car items) curX)) tolX))
      (progn
        (if cur (setq buckets (append buckets (list (list curX cur)))))
        (setq curX (car items) cur (list it)))
      (setq cur (append cur (list it))))
  )
  (if cur (setq buckets (append buckets (list (list curX cur)))))
  (setq result '())
  (foreach b buckets
    (setq items (cadr b))
    (setq items (vl-sort items (function (lambda (a b) (> (cadr (cdr a)) (cadr (cdr b)))))))
    (setq result (append result items)))
  result
)


;; --- 全域變數預設 CC---
(if (not *CC_tol*)  (setq *CC_tol* 0.5)) 
(if (not *CC_Tags*) (setq *CC_Tags* '("M00" "22/2" "PHNX")))

;; --- 基礎屬性讀寫函式 (VLA 模式) ---
(defun CC_GetAtt (en tag / obj result done)
  (if (null en) (setq result nil)
    (progn
      (setq obj (vlax-ename->vla-object en))
      (if (and (= (vla-get-ObjectName obj) "AcDbBlockReference")
               (= :vlax-true (vla-get-HasAttributes obj)))
          (foreach a (vlax-invoke obj 'GetAttributes)
            (if (and (null done) (= (strcase (vla-get-TagString a)) (strcase tag)))
                (setq result (vla-get-TextString a) done t))))))
  (if result (vl-string-trim " " result) nil))

(defun CC_SetAtt (en tag val / obj done)
  (if (and en val)
    (progn
      (setq obj (vlax-ename->vla-object en))
      (foreach a (vlax-invoke obj 'GetAttributes)
        (if (and (null done) (= (strcase (vla-get-TagString a)) (strcase tag)))
            (progn (vla-put-TextString a val) (setq done t))))
      (vla-update obj) done)))





;; --- 主指令：TB ---
(defun c:TB (/ mode prefix start ss i e lst new cnt ent loop)
  (prompt "\n=== TB：標籤遞增 (固定位數2 / 步距1) ===")

  ;; 1) 先設定參數 (模式、前綴、起始號) [修正需求]
  (initget "CL P")
  (setq mode (getkword (strcat "\n[1/3] 選擇模式 [CL(同欄)/P(點選)] <" *TBlinc_mode* ">: ")))
  (if (null mode) (setq mode *TBlinc_mode*) (setq *TBlinc_mode* mode))

  (setq prefix (getstring T (strcat "\n[2/3] 輸入前綴 <" *TBlinc_prefix* ">: ")))
  (if (= prefix "") (setq prefix *TBlinc_prefix*) (setq *TBlinc_prefix* prefix))
  
  (setq start (getint (strcat "\n[3/3] 輸入起始號 <" (itoa *TBlinc_start*) ">: ")))
  (if (null start) (setq start *TBlinc_start*) (setq *TBlinc_start* start))

  ;; 2) 根據模式執行
  (setq cnt 0 i 0)
  (cond
    ;; --- 模式 P：點選順序 ---
    ((= mode "P")
     (setq loop t)
     (prompt "\n模式：[點選] - 請依序選取物件，按 Enter 結束。")
     (while loop
       (setvar "ERRNO" 0)
       (setq ent (entsel (strcat "\n下一編號 " prefix (_zpad2 (+ start i)) " - 請選擇物件: ")))
       (cond
         ((and ent (car ent))
          (setq new (strcat prefix (_zpad2 (+ start i))))
          (if (_update-TB-vla (car ent) new *TBlinc_tag*)
              (setq i (1+ i) cnt (1+ cnt))
              (princ "\n[錯誤] 此物件不含指定標籤。")))
         ((= (getvar "ERRNO") 52) (setq loop nil)) ; Enter 退出
         (t (princ "\n未選到物件，請繼續...")) ; 點空不停止
       ))
    )

    ;; --- 模式 CL：同欄排序 ---
    ((= mode "CL")
     (prompt "\n模式：[同欄] - 請框選物件：")
     (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
     (if ss
       (progn
         (setq i 0 lst '())
         (repeat (sslength ss)
           (setq e (ssname ss i)
                 lst (cons (cons e (_pt-of e)) lst)
                 i (1+ i)))
         (setq lst (_sort-CL lst))
         (setq i 0)
         (foreach pair lst
           (setq e (car pair)
                 new (strcat prefix (_zpad2 (+ start i))))
           (if (_update-TB-vla e new *TBlinc_tag*) (setq cnt (1+ cnt)))
           (setq i (1+ i)))
       )
       (prompt "\n未選取物件。")
     )
    )
  )

  (prompt (strcat "\n指令結束。共成功更新 " (itoa cnt) " 個標籤。"))
  (princ)
)

;; --- 設定指令：TB_SET ---
(defun c:TB_SET (/ tg ct)
  (prompt "\n=== TB_SET：進階參數設定 ===")
  
  (setq tg (getstring T (strcat "\n指定屬性 Tag <" *TBlinc_tag* ">: ")))
  (if (and tg (/= tg "")) (setq *TBlinc_tag* tg))
  
  (setq ct (getreal (strcat "\n同欄容差 (僅限 CL 模式) <" (rtos *TBlinc_coltol* 2 2) ">: ")))
  (if ct (setq *TBlinc_coltol* ct))

  (prompt (strcat "\n設定已更新：Tag=" *TBlinc_tag* " / 容差=" (rtos *TBlinc_coltol* 2 2)))
  (princ)
)


;; --- 主指令：CC ---
(defun c:CC (/ ent ename p1 p2 p1a p1b p2a p2b ss1 ss2 b1 b2 sblk dblk tags v loop x1 x2 pick vals target_loop err)
  (setq tags *CC_Tags* loop t)
  (princ "\n--- CC: 屬性同步 (左源右標 / 垂直轉 CCC 模式) ---")

  (while loop
    (setvar "ERRNO" 0)
    (setq ent (entsel "\n選取連接線段 [按 Enter 結束]: "))
    (cond
      ((and ent (setq ename (car ent)) 
            (member (cdr (assoc 0 (entget ename))) '("LWPOLYLINE" "POLYLINE" "LINE")))
       
       (setq p1 (vlax-curve-getStartPoint ename)
             p2 (vlax-curve-getEndPoint ename))

       ;; 建立端點偵測區域 (建立 ss1, ss2 僅用於判斷 X 座標)
       (setq p1a (mapcar '- p1 (list *CC_tol* *CC_tol* 0))
             p1b (mapcar '+ p1 (list *CC_tol* *CC_tol* 0))
             p2a (mapcar '- p2 (list *CC_tol* *CC_tol* 0))
             p2b (mapcar '+ p2 (list *CC_tol* *CC_tol* 0)))

       (setq ss1 (ssget "_C" p1a p1b '((0 . "INSERT")))
             ss2 (ssget "_C" p2a p2b '((0 . "INSERT"))))

       (if (and ss1 ss2)
           (progn
             (setq b1 (ssname ss1 0)
                   b2 (ssname ss2 0)
                   x1 (car (cdr (assoc 10 (entget b1))))
                   x2 (car (cdr (assoc 10 (entget b2)))))

             (cond
               ;; --- 狀況 A: 非垂直線 (自動偵測左源右標) ---
               ((not (equal x1 x2 0.001))
                (if (< x1 x2) (setq sblk b1 dblk b2) (setq sblk b2 dblk b1))
                (foreach tg tags
                  (setq v (CC_GetAtt sblk tg))
                  (if v (CC_SetAtt dblk tg v)))
                (princ "\n>> 自動同步成功 (左至右)。"))

               ;; --- 狀況 B: 垂直線 (直接轉 CCC 手動選取模式) ---
               (t 
                (princ "\n[垂直線偵測] X 相同，請改用手動選取：")
                (setq sblk nil)
                ;; 步驟 1: 選取來源 (完全自由點選，不受限於端點)
                (while (null sblk)
                  (setq pick (entsel "\n   -> [CCC模式] 選取「來源」圖塊: "))
                  (if (and pick (setq sblk (car pick)))
                      (if (/= (cdr (assoc 0 (entget sblk))) "INSERT") 
                          (progn (princ "\n[提示] 請選取一個圖塊。") (setq sblk nil)))
                      (if (= (getvar "ERRNO") 52) (setq sblk 'exit))))
                
                (if (and sblk (/= sblk 'exit))
                    (progn
                      ;; 讀取來源資料
                      (setq vals nil)
                      (foreach tg tags (setq vals (append vals (list (CC_GetAtt sblk tg)))))
                      
                      ;; 步驟 2: 選取目標 (完全自由點選)
                      (princ "\n   >> 來源資料已鎖定，請選取「目標」圖塊。")
                      (setq target_loop t)
                      (while target_loop
                        (setvar "ERRNO" 0)
                        (setq pick (entsel "\n   -> [CCC模式] 選取「目標」圖塊: "))
                        (setq err (getvar "ERRNO"))
                        (cond
                          ((and pick (setq dblk (car pick)))
                           (if (= (cdr (assoc 0 (entget dblk))) "INSERT")
                               (progn
                                 (mapcar '(lambda (tg vv) (if vv (CC_SetAtt dblk tg vv))) tags vals)
                                 (princ "\n>> 同步成功。")
                                 (setq target_loop nil))
                               (princ "\n[提示] 選取的物件不是圖塊，請重新選取目標。")))
                          ((= err 52) 
                           (princ "\n>> 放棄本次垂直同步。")
                           (setq target_loop nil))
                          (t (princ "\n[提示] 未選到目標，請點選目標圖塊（來源資料仍鎖定中）..."))
                        )
                      )
                    )
                )
               )
             )
           )
           (princ "\n[錯誤] 線段端點偵測失敗，請確認圖塊在端點或調整容差。")
       )
      )
      ((= (getvar "ERRNO") 52) (setq loop nil)) 
    )
  )
  (princ "\n已退出 CC。")
  (princ)
)

(defun c:CC_SET (/ ct i inp tags)
  (prompt "\n=== CC_SET：同步參數設定 ===")
  (setq ct (getreal (strcat "\n端點偵測誤差容許值 <" (rtos *CC_tol* 2 2) ">: ")))
  (if ct (setq *CC_tol* ct))
  (setq i 0 tags '())
  (while (< i 3)
    (setq inp (getstring T (strcat "\n同步 TAG " (itoa (1+ i)) " <" (nth i *CC_Tags*) ">: ")))
    (setq tags (append tags (list (if (= inp "") (nth i *CC_Tags*) inp))))
    (setq i (1+ i)))
  (setq *CC_Tags* tags) (princ))


(princ "\n[載入成功] CC (線段自動選模式) | CC_SET (設定) | CCC (手動模式) | CCC_SET(設定)")
(princ "\n[載入成功] TB (編號) | TB_SET (設定)")
(princ)