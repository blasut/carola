# Carola

To connect to poloniex API.

## Usage

```common-lisp
(ql:quickload 'osicat)

(setf (osicat:environment-variable "api_key") your_key_here)
(setf (osicat:environment-variable "api_secret") your_secret_here)

(push #p"APPDIR" asdf:*central-registry*)

(ql:quickload :carola)

```

## TODO

- Implement the rest of the methods.
- Implement optional arguments.
- Generate documentation from code.

![Carola](http://i.ytimg.com/vi/Jbuczj_elfM/maxresdefault.jpg)
