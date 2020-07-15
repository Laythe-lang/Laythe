import Math from 'std/math';
import Stdio from 'std/io/stdio';

let stdout = Stdio.stdout;

let DEAD = 0;
let ALIVE = 1;
let DEFAULT = 20;

class Cell {
  init(status) {
    self.status = status;
  }

  static alive() {
    return Cell(ALIVE);
  }

  static dead() {
    return Cell(DEAD);
  }

  toggle() {
    self.status = 1 - self.status;
  }

  isAlive() {
    return self.status == ALIVE;
  }

  render() {
    if (self.isAlive()) {
      return '□';
    }

    return '■';
  }
}

class Universe {
  static new() {
    let cells = (DEFAULT * DEFAULT * 2)
      .times()
      .reduce([], |list, _| {
        if (Math.rand() > 0.5) {
          list.push(Cell.alive());
        } else {
          list.push(Cell.dead());
        }

        return list;
      });

    return Universe(DEFAULT * 2, DEFAULT, cells);
  }

  init(width, height, cells) {
    self.width = width;
    self.height = height;
    self.cells = cells;
  }

  getIndex(row, col) {
    return (row * self.width + col);
  }

  liveNeighbors(row, col) {
    let count = 0;
    for (let deltaRow in [self.height - 1, 0, 1]) {
      for (let deltaCol in [self.width - 1, 0, 1]) {
        if (deltaRow != 0 or deltaCol != 0) {
          let neighborRow = Math.rem(row + deltaRow, self.height);
          let neighborCol = Math.rem(col + deltaCol, self.width);

          let idx = self.getIndex(neighborRow, neighborCol);
          count = count + self.cells[idx].status;
        }
      }
    }
    return count;
  }

  runSteps(n) {
    n.times().each(|i| {
      self.tick();
      self.render();

      stdout.writeln('');
      stdout.writeln('');
    });
  }

  tick() {
    let next = [];
    for (let cell in self.cells) {
      next.push(cell);
    }

    for (let row in self.height.times()) {
      for (let col in self.width.times()) {
        let idx = self.getIndex(row, col);
        let cell = self.cells[idx];
        let liveNeighbors = self.liveNeighbors(row, col);

        let nextCell = cell;
        if (cell.isAlive()) {
          if (liveNeighbors < 2 or liveNeighbors > 3) {
            nextCell = Cell.dead();
          } else {
            nextCell = Cell.alive();
          }
        } else {
          if (liveNeighbors == 3) {
            nextCell = Cell.alive();
          } else {
            nextCell = Cell.dead();
          }
        }

        next[idx] = nextCell;
      }
    }

    self.cells = next;
  }

  render() {
    for (let row in self.height.times()) {
      for (let col in self.width.times()) {
        let idx = self.getIndex(row, col);
        let cell = self.cells[idx];

        stdout.write(cell.render());
      }

      stdout.writeln('');
    }
  }
}

let universe = Universe.new();
universe.runSteps(30);
