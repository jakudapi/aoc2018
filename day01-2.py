from time import time

def modify(acc: int, op: str, value: int) -> int:
  if op == "+":
    return acc + value
  else:
    return acc - value

if __name__ == "__main__":

  start_t = time()

  with open("day01input.txt", 'r') as fp:
    input = fp.read()

  input = input.strip().split("\n")

  for index, line in enumerate(input):
    input[index] =  (line[0], int(line[1:]))


  frequency = 0
  tracker = {0}
  end_of_input = True
  
  while end_of_input:
    for sign, value in input:
      frequency = modify(frequency, sign, value)
      if frequency in tracker: 
        end_of_input = False
        break
      else:
        tracker.add(frequency)
  
  end_t = time()
  print(frequency)
  print(f"this took {round(end_t - start_t,5)} secs")

