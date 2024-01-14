##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile for glados :)
##

NAME = glados

all: $(NAME)

$(NAME):
	stack build --copy-bins . --local-bin-path .
	mv glados-exe $(NAME)

run_tests:
	stack test

clean:
	stack clean

fclean:
	stack clean
	rm -f $(NAME)

re: fclean all
