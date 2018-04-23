;;; beginning-json.el --- Beginning json processing  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Matthias

;; Author: Matthias <mpfeifer77@gmail.com>
;; Keywords: json, json.el

(setq jsonstr "{\"cols\":[\"Datum\",\"Wert\"], \"data\":[[\"25.06.17\",\"0.02243738518587\"],[\"06.11.17\",\"0.37698196616106\"],[\"23.03.16\",\"0.21900616480905\"]]}")

(let* ((json-object-type 'hash-table)
       (json-array-type 'list)
       (json-key-type 'string)
       (jsonobj (json-read-from-string jsonstr)))
  (let ((data (gethash "data" jsonobj)))
    (dolist (dataitem data)
      (insert (format "\n%s, %s" (car dataitem) (car (cdr dataitem)))))))

