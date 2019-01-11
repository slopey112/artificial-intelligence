#include <stdio.h>
#include <stdlib.h>

int FAIL = -1;
char board[9] = {' ', ' ', ' ',
                 ' ', ' ', ' ',
                 ' ', ' ', ' '};
int pos[21] = {0, 1, 2,
               3, 4, 5,
               6, 7, 8,
               2, 4, 6,
               0, 3, 6,
               1, 4, 7,
               2, 5, 8};
int sides[4] = {1, 3, 5, 7};
int corners[4] = {0, 2, 6, 8};


void putboard(int index, char sel) {
  board[index] = sel;
}

void delboard(int index) {
  board[index] = ' ';
}

int isopen(int index) {
  if (board[index] == ' ') return 1;
  else return FAIL;
}

void printboard() {
  char a = 'a';
  printf(" %c | %c | %c\n"
    "___|___|___\n"
    " %c | %c | %c\n"
    "___|___|___\n"
    " %c | %c | %c\n"
    "   |   |\n",
    board[0], board[1], board[2],
    board[3], board[4], board[5],
    board[6], board[7], board[8]);
}

int almostwin(int a, int b, int c) {
  int x = 0;
  int o = 0;
  int vals[3] = {a, b, c};
  for (int i = 0; i < 3; i++) {
    if (board[vals[i]] == 'X') x++;
    else if (board[vals[i]] == 'O') o++;
  }
  if (x == 2 && o == 0) return 2;
  else if (o == 2 && x == 0) return 1;
  else return FAIL;
}

int canwin(int index) {
  int ck[3] = {0};
  int ct = 0;
  int res = 1;
  putboard(index, 'X');
  for (int i = 0; i < 21; i++) {
    ck[ct] = pos[i];
    ct++;
    if ((i + 1) % 3 == 0) {
      for (int j = 0; j < 3; j++) {
        if (ck[j] == index) {
          if (almostwin(ck[0], ck[1], ck[2]) != FAIL) {
            delboard(index);
            return 1;
          }
        }
      }
      ck[0] = 0;
      ck[1] = 0;
      ck[2] = 0;
      ct = 0;
    }
  }
  delboard(index);
  return FAIL;
}

int * checkcorners() {
  for (int i = 0; i < 4; i++) {
    if (isopen(corners[i]) == 1 && canwin(corners[i]) == 1) return &(corners[i]);
  }
  return &FAIL;
}

int * checksides() {
  for (int i = 0; i < 4; i++) {
    if (isopen(sides[i]) == 1 && canwin(sides[i]) == 1) return &(sides[i]);
  }
  return &FAIL;
}

void checkwin(int a, int b, int c) {
  if (board[a] == board[b] && board[b] == board[c] && board[a] == board[c] && board[a] != ' ') {
    if (board[a] == 'X') {
      printf("You lost.\n");
      exit(0);
    } else {
      printf("You won.\n");
      exit(0);
    }
  }
}

void checktie() {
  int check = 9;
  for (int i = 0; i < 9; i++) {
    if (board[i] == ' ') {
      check--;
    }
  }
  if (check == 9) {
    printf("Tie.\n");
    exit(0);
  }
}

void checkall() {
  checktie();
  int ck[3] = {0};
  int ct = 0;
  for (int i = 0; i < 21; i++) {
    ck[ct] = pos[i];
    ct++;
    if ((i + 1) % 3 == 0) {
      checkwin(ck[0], ck[1], ck[2]);
      ck[0] = 0;
      ck[1] = 1;
      ck[2] = 2;
      ct = 0;
    }
  }
}

int * xgoingtowin() {
  int ck[3] = {0};
  int ct = 0;
  for (int i = 0; i < 21; i++) {
    ck[ct] = pos[i];
    ct++;
    if ((i + 1) % 3 == 0) {
      if (almostwin(ck[0], ck[1], ck[2]) == 2) {
        if (board[pos[i-2]] == ' ') {
          return &(pos[i-2]);
        } else if (board[pos[i-1]] == ' ') {
          return &(pos[i-1]);
        } else if (board[pos[i]] == ' ') {
          return &(pos[i]);
        }
      }
      ck[0] = 0;
      ck[1] = 0;
      ck[2] = 0;
      ct = 0;
    }
  }
  return &FAIL;
}

int * ogoingtowin() {
  int ck[3] = {0};
  int ct = 0;
  for (int i = 0; i < 21; i++) {
    ck[ct] = pos[i];
    ct++;
    if ((i + 1) % 3 == 0) {
      if (almostwin(ck[0], ck[1], ck[2]) == 1) {
        if (board[ck[0]] == ' ') {
          return &(pos[i-2]);
        } else if (board[ck[1]] == ' ') {
          return &(pos[i-1]);
        } else if (board[ck[2]] == ' ') {
          return &(pos[i]);
        }
      }
      ck[0] = 0;
      ck[1] = 0;
      ck[2] = 0;
      ct = 0;
    }
  }
  return &FAIL;
}

void lastresort() {
  for (int i = 0; i < 9; i++) {
    if (isopen(i)) {
      putboard(i, 'X');
      break;
    }
  }
}

void bot() {
  int * xret = xgoingtowin();
  int * oret = ogoingtowin();
  int cent = isopen(4);
  int * corn = checkcorners();
  int * side = checksides();
  if (*xret != FAIL) {
    putboard(*xret, 'X');
  } else if (*oret != FAIL) {
    putboard(*oret, 'X');
  } else if (cent != FAIL) {
    putboard(4, 'X');
  } else if (*corn != FAIL) {
    putboard(*corn, 'X');
  } else if (*side != FAIL) {
    putboard (*side, 'X');
  } else {
    lastresort();
  }
}

int main() {
  printf("---------------------\n");
  printf("     TIC TAC TOE     \n");
  printf("---------------------\n");
  printf("For inputting selections, you must enter a number representing the index of your selection. (EG 5 would refer to the center slot.)\n");
  printboard();
  for (;;) {
    int in;
    printf("\nEnter a number between 1 and 9: ");
    scanf("%i", &in);
    printf("\n");
    if (in > 9 || in < 1 || isopen(in-1) == FAIL) {
      continue;
    }
    putboard((in-1), 'O');
    printboard();
    checkall();
    bot();
    printf("\n");
    printboard();
    checkall();
  }
}
