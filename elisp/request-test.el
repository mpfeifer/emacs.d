;;; request-test.el --- Some tests with package request.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Matthias

;; Author: Matthias <mpfeifer77@gmail.com>

(setq response (request "http://httpbin.org/ip"
         :type "GET"
         :parser 'json-read))
