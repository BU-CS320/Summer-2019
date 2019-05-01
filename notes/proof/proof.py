def is_greater_then_or_eq_to_zero(n):
    if n == 0:
        print("0 >= 0, by Rule 1")
    else:
        is_greater_then_or_eq_to_zero(n-1)
        print(n-1, "+ 1 >= 0, by Rule 2")


is_greater_then_or_eq_to_zero(100)
