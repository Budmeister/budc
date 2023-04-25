
# import pip
# pip.main(["install", "colorama"])

from colorama import Fore, Back, Style

def yellow(string):
    return Fore.YELLOW + str(string) + Style.RESET_ALL

def red(string):
    return Fore.RED + str(string) + Style.RESET_ALL

def blue(string):
    return Fore.BLUE + str(string) + Style.RESET_ALL

def green(string):
    return Fore.GREEN + str(string) + Style.RESET_ALL

output_dir = "output.txt"

trace = []
debug = []
info = []
warn = []
error = []
other = []
usage = None
logging_level = None
logging_options = None
grammar = None
firsts = None
states = None
action_table = None
actions = None
stack = None
syntax_tree = None

def read_usage(lines, index):
    if index >= len(lines):
        return index
    if lines[index].startswith("Usage: "):
        global usage
        usage = lines[index]
        index += 1
    return index

def read_logging_level(lines, index):
    if index >= len(lines):
        return index
    global logging_level
    if lines[index].startswith("Logging level of "):
        logging_level = lines[index].split()[3]
        print(green("Found logging level"))
        index += 1
    elif lines[index].startswith("No logging level selected,"):
        logging_level = lines[index].split()[4]
        print(green("Found logging level"))
        index += 1
    else:
        print(red("Did not find logging level"))
    return index

def read_logging_options(lines, index):
    if index >= len(lines):
        return index
    words = lines[index].split()
    if words[0] == "LoggingOptions":
        if words[1] != "{":
            print(yellow("Error reading logging options. Did not find left squiggly."))
            index += 1
            return index
        else:
            print(green("Found logging options"))
        global logging_options
        logging_options = {}
        for i in range(2, len(words)-1, 2):
            if words[i][-1] != ":":
                print(yellow(f"Error reading logging options. Word does not end in colon: \"{words[i]}\""))
            logging_options[words[i][:-1]] = bool(words[i+1][:-1])
        index += 1
    else:
        print(red("Did not find logging options"))
    return index

def read_grammar(lines, index):
    if index >= len(lines):
        return index
    if lines[index] == "Grammar:":
        print(green("Found grammar"))
        index += 1
        if index >= len(lines):
            return index
        global grammar
        grammar = {}
        while lines[index] != "End grammar":
            fro, to = lines[index].split(" -> ")
            if fro in grammar:
                grammar[fro].append(to)
            else:
                grammar[fro] = [to]
            index += 1
            if index >= len(lines):
                return index
        index += 1
    else:
        print(red("Did not find grammar"))
    return index

def read_firsts(lines, index):
    if index >= len(lines):
        return index
    if lines[index] == "Firsts:":
        print(green("Found firsts"))
        index += 1
        if index >= len(lines):
            return index
        global firsts
        firsts = {}
        while lines[index] != "End firsts":
            n, f = lines[index].split(": ")
            firsts[n] = f.split()
            index += 1
            if index >= len(lines):
                return index
        index += 1
    else:
        print(red("Did not find firsts"))
    return index

def read_state_transitions(lines, index):
    if index >= len(lines):
        return index
    if lines[index] == "State Transitions:":
        print(green("Found state transitions"))
        index += 1
        if index >= len(lines):
            return index
        while lines[index] != "End state transitions":
            # Don't do anything for state transitions
            index += 1
            if index >= len(lines):
                return index
        index += 1
    else:
        print(red("Did not find state transitions"))
    return index

def read_states(lines, index):
    if index >= len(lines):
        return index
    if lines[index] == "States:":
        print(green("Found states"))
        index += 1
        if index >= len(lines):
            return index
        global states
        states = []
        while lines[index] != "End states":
            _, line = lines[index].split(": [")
            line = line[:-1]
            words = line.split()
            try:
                next_arrow = words.index("->")
            except ValueError:
                next_arrow = -1
            state = []  # state = [(der, look), ...]
            while 1:
                arrow = next_arrow
                if arrow == -1:
                    break
                try:
                    next_arrow = words.index("->", arrow+1)
                except ValueError:
                    next_arrow = -1
                der = words[arrow-1] + " -> " + words[arrow+1][:-1] # last character in "to" should be comma
                if next_arrow != -1:
                    look = " ".join(words[arrow+2:next_arrow-1])[:-1]
                else:
                    look = " ".join(words[arrow+2:])
                state.append((der, look))
            states.append(state)

            index += 1
            if index >= len(lines):
                return index
        index += 1
    else:
        print(red("Did not find states"))
    return index

def read_action_table(lines, index):
    if index >= len(lines):
        return index
    if lines[index] == "Action Table:":
        print(green("Found action table"))
        index += 1
        if index >= len(lines):
            return index
        symbols = lines[index].split("\t")[1:]
        index += 1
        if index >= len(lines):
            return index
        global action_table
        action_table = []   # list<dict<symbol, action>>    # action can be '[r1, s2]'
        while lines[index] != "End action table":
            actions = lines[index].split("\t")[1:]
            actions = {sym: action for sym, action in zip(symbols, actions) if action != ""}
            action_table.append(actions)
            index += 1
            if index >= len(lines):
                return index
        index += 1
    else:
        print(red("Did not find action table"))
    return index

def read_actions(lines, index):
    if index >= len(lines):
        return index
    if lines[index] == "Actions:":
        print(green("Found actions"))
        index += 1
        if index >= len(lines):
            return index
        global actions
        global stack
        actions = []
        while lines[index] != "End actions":
            if lines[index].startswith("Stack: "):
                stack = lines[index][len("Stack: "):]
                stack = stack[1:-1]
                if len(stack) > 0:
                    stack = [int(x) for x in stack.split(",")]
                else:
                    stack = []
            actions.append(lines[index])
            index += 1
            if index >= len(lines):
                return index
        index += 1
    else:
        print(red("Did not find actions"))
    return index

def read_syntax_tree(lines, index):
    if index >= len(lines):
        return index
    if lines[index] == "Syntax Tree:":
        print(green("Found syntax tree"))
        index += 1
        if index >= len(lines):
            return index
        global syntax_tree
        syntax_tree = []
        while lines[index] != "End syntax tree":
            syntax_tree.append(lines[index])
            index += 1
            if index >= len(lines):
                return index
        index += 1
    else:
        print(green("Did not find syntax tree"))
    return index

def read_output(dir):
    with open(dir, "r") as file:
        lines = file.readlines()
    logs = [
        ("TRACE", trace),
        ("DEBUG", debug),
        ("INFO", info),
        ("WARN", warn),
        ("ERROR", error),
    ]
    to_read = []
    for line in lines:
        line = line.strip()
        for log_name, log in logs:
            if line.startswith(log_name + " - "):
                log.append(line)
                if log is not error and log is not warn:
                    to_read.append(line[len(log_name + " - "):])
                break
        else:
            other.append(line)
            to_read.append(line)
    
    index = 0
    index = read_usage(to_read, index)
    index = read_logging_level(to_read, index)
    index = read_logging_options(to_read, index)
    index = read_grammar(to_read, index)
    index = read_firsts(to_read, index)
    index = read_state_transitions(to_read, index)
    index = read_states(to_read, index)
    index = read_action_table(to_read, index)
    index = read_actions(to_read, index)
    index = read_syntax_tree(to_read, index)

def print_log(type):
    logs = {
        "trace": trace,
        "debug": debug,
        "info": info,
        "warn": warn,
        "error": error,
        "other": other,
    }
    if type in logs:
        if len(logs[type]) < 500 or input(f"There are {len(logs[type])} {type} logs. Still print? (y/n) ") in ("Y", "y"):
            [print(x) for x in logs[type]]
    else:
        print(f"Unknown log type: {red(type)}")

def print_logging_level(*args):
    if logging_level is not None:
        print(logging_level)
    else:
        print(yellow("No logging level found"))

def print_logging_options(*args):
    if logging_options is not None:
        print("{")
        for k, v in logging_options.items():
            print(f"\t\"{k}\": {v}")
        print("}")
    else:
        print(yellow("No logging options found"))

def print_ders(*args):
    if grammar is not None:
        if len(args) > 0:
            fros = args
        else:
            fros = grammar.keys()
        for fro in fros:
            if fro in grammar:
                tos = grammar[fro]
                [print(f"{fro} -> {to}") for to in tos]
            else:
                print(yellow(f"No derivation for {fro}"))
    else:
        print(yellow("No grammer found"))

def print_firsts(*args):
    if firsts is not None:
        if len(args) > 0:
            syms = args
        else:
            syms = firsts.keys()
        for sym in syms:
            if sym in firsts:
                print(f"{sym}: {' '.join(firsts[sym])}")
            else:
                print(yellow(f"No firsts found for {sym}"))
    else:
        print(yellow("No firsts found"))

def print_state_pretty(state_num):
    if state_num >= len(states):
        print(yellow(f"State num too big: {state_num}"))
    else:
        state = states[state_num]
        print(red(f"State {state_num}"))
        for der, look in state:
            print(f"\tder: {pad_back(blue(der), 50)}, lookahead: {look[1:-1]}")
        print()

def print_state(*args):
    if states is not None:
        if len(args) > 0:
            state_nums = args
        else:
            if len(states) < 100 or input(f"There are {len(states)} states. Do you still want to print them all? (y/n) ") in ("Y", "y"):
                state_nums = range(len(states))
            else:
                return
        for state_num in state_nums:
            try:
                state_num = int(state_num)
            except:
                print(yellow(f"Invalid state num: {state_num}"))
                continue
            print_state_pretty(int(state_num))
    else:
        print("No states found")

def get_action(*args):
    if action_table is not None:
        if len(args) > 0:
            if len(args) == 1:
                state = int(args[0])
                if state >= len(action_table):
                    print(yellow(f"State number too big: {state}"))
                    return
                actions = action_table[state]
                for k, v in actions.items():
                    print(f"{blue(k)}: {v}")
            else:
                states = [int(x) for x in args[0::2]]
                syms = args[1::2]
                for state, sym in zip(states, syms):
                    if state >= len(action_table):
                        print(yellow(f"State number too big: {state}"))
                        continue
                    action = action_table[state]
                    if sym in action:
                        action = actions[sym]
                    else:
                        action = "None"
                    print(f"{red(state)}({blue(sym)}): {action}")
        else:
            print("Usage: action [state]")
            print("Usage: action [state1] [sym1] [state2] [sym2] ...")
    else:
        print("No action table found")

def print_actions(*args):
    if actions is not None:
        [print(x) for x in actions]
    else:
        print("No actions found")

def print_stack(*args):
    if stack is not None:
        print(f"Stack: {stack}")
    else:
        print("No stack found")

def print_stack_states(*args):
    if stack is not None:
        for state in stack:
            print_state_pretty(state)
    else:
        print("No stack found")

def pad_back(string, length):
    while len(string) < length:
        string += " "
    return string

def print_help(*args):
    length = 10
    for command in commands:
        print(f"\t{pad_back(command, length)} - {commands[command][1]}")

commands = {
    "trace":            (lambda *args: print_log("trace"),      "Print trace log messages"),
    "debug":            (lambda *args: print_log("debug"),      "Print debug log messages"),
    "info":             (lambda *args: print_log("info"),       "Print info log messages"),
    "warn":             (lambda *args: print_log("warn"),       "Print warn log messages"),
    "error":            (lambda *args: print_log("error"),      "Print error log messages"),
    "other":            (lambda *args: print_log("other"),      "Print other log messages"),
    "llevel":           (print_logging_level,                   "Print the logging level"),
    "loptions":         (print_logging_options,                 "Print logging options"),
    "ders":             (print_ders,                            "Print all derivations for the given symbol(s)"),
    "first":            (print_firsts,                          "Prints the first set for the given symbol(s)"),
    "state":            (print_state,                           "Pretty print the given state(s)"),
    "action":           (get_action,                            "Print the action for the given state and symbol OR all actions for a state"),
    "actions":          (print_actions,                         "Print the actions taken in parsing"),
    "stack":            (print_stack,                           "Print the final stack of the parser"),
    "stackstates":      (print_stack_states,                    "Pretty print all the states on the stack"),
    "exit":             (lambda *args: exit(),                  "Quit the analyzer"),
    "help":             (print_help,                            "Print this help message"),
}

def execute(command):
    command, *args = command.split()
    if command not in commands:
        print(f"Unknown command: {command}")
        command = "help"
    commands[command][0](*args)

if __name__ == "__main__":
    print("Output Analyzer")
    print()
    print(f"Getting output from file: {output_dir}")
    read_output(output_dir)

    while 1:
        command = input(">> ")
        execute(command)
    