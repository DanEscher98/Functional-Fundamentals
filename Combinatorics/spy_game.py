#!/usr/bin/python3

from math import factorial

from scipy.special import comb


def possible_games(cards, board_len):
    board = board_len ** 2
    words = comb(cards, board, exact=True)
    assassin = 0 if (board % 3 == 0) else 1
    card = int((board - assassin) / 3)
    boards = int(factorial(board) / (factorial(card - 1) ** 3 * card ** 2 * (card + 1)))
    # / (2 * prod(map(factorial, [card - 1, card, card + 1])))
    return words * boards
