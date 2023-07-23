from_dir = "src/bud.rs"
to_dir = "src/bud.rs"

chars_new = None
left_paren = "("
right_paren = ")"

# str_to_match = "Node::NonTm("
# new_str_open = "Node::NonTm { n: "
# new_str_close = ", range }"

str_to_match = "Node::Tm("
new_str_open = "Node::Tm { t: "
new_str_close = ", range }"

def find_next(str_to_match_open):
    global index
    global index_new
    global running
    global open_index
    global close_index
    global open_new
    global close_new
    
    if index + len(str_to_match_open) >= len(chars):
        running = False
        return
    while chars[index:(index + len(str_to_match_open))] != str_to_match_open:
        if chars[index] != chars_new[index_new]:
            print("Expected", chars[index], "but found", chars_new[index_new])
        if index + len(str_to_match_open) >= len(chars):
            running = False
            return
        index += 1
        index_new += 1

    to_check_close = index + len(str_to_match_open)
    to_check_close_new = index_new + len(str_to_match_open)
    opening_parens = 0
    while True:
        if chars[to_check_close] != chars_new[to_check_close_new]:
            print("Expected", chars[to_check_close], "but found", chars_new[to_check_close_new])
        if chars[to_check_close] == left_paren:
            opening_parens += 1
        elif chars[to_check_close] == right_paren:
            if opening_parens == 0:
                break
            opening_parens -= 1
        to_check_close += 1
        to_check_close_new += 1
    open_index = index
    close_index = to_check_close
    open_new = index_new
    close_new = to_check_close_new

def replace_n_args(in_between_str):
    parts = in_between_str.split(",")
    if len(parts) != 2:
        print("Not able to replace_n_args for in_between_str:", in_between_str)
        return in_between_str
    first, second = parts
    return first + ", children:" + second

def replace(n):
    global index
    global chars_new
    global index_new
    global close_index
    global open_new
    global close_new
    in_between_str = chars_new[(open_new + len(str_to_match)):close_new]
    if n:
        new_in_between_str = replace_n_args(in_between_str)
    else:
        new_in_between_str = in_between_str
    chars_new = chars_new[:open_new] + new_str_open \
            + new_in_between_str + new_str_close \
            + chars_new[(close_new + len(right_paren)):]
    index = close_index + len(right_paren)
    index_new = close_new + len(new_str_open) - len(str_to_match) + len(new_in_between_str) - len(in_between_str) + len(new_str_close)

def run(n):
    global from_dir
    global to_dir
    global chars
    global chars_new
    global left_paren
    global right_paren
    global str_to_match
    global new_str_open
    global new_str_close
    global index
    global index_new
    global open_index
    global close_index
    global open_new
    global close_new
    global running

    with open(from_dir, "r") as file:
        chars = file.read()

    chars_new = chars
    running = True

    while running:
        find_next(str_to_match)
        line_num = chars[:open_index].count("\n") + 1
        print("Found at", index, "on line", line_num, "-", chars[open_index:close_index+1])
        replace(n)
        index += 1
        index_new += 1

    print("Finished! Saving to file...")

    with open(to_dir, "w") as file:
        file.write(chars_new)

if __name__ == "__main__":
    from_dir = "src/bud_old.rs"
    to_dir = "src/bud_step_1.rs"

    left_paren = "("
    right_paren = ")"

    # Initialize globals
    index = 0
    index_new = 0
    open_index = None
    close_index = None
    open_new = None
    close_new = None
    running = True

    str_to_match = "Node::NonTm("
    new_str_open = "Node::NonTm { n: "
    new_str_close = ", range }"

    index = 0

    run(True)

    # Initialize globals
    index = 0
    index_new = 0
    open_index = None
    close_index = None
    open_new = None
    close_new = None
    running = True

    str_to_match = "Node::Tm("
    new_str_open = "Node::Tm { t: "
    new_str_close = ", range }"

    from_dir = "src/bud_step_1.rs"
    to_dir = "src/bud.rs"

    run(False)
