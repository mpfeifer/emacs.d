;;; request-test.el --- Some tests with package request.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Matthias

;; Author: Matthias <mpfeifer77@gmail.com>


(request "http://de.wikipedia.org"
         :type "GET"
         :parser 'buffer-string
         :success (lambda (&key data &allow-other-keys)
              (when data
                (with-current-buffer (get-buffer-create "*request demo*")
                  (erase-buffer)
                  (insert data)
                  (pop-to-buffer (current-buffer))))))
