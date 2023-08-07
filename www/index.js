import { Universe, BasicRule, EmptyState} from "simple-automata";
import { memory } from "simple-automata/simple_automata_bg";
import "./elm.js"

const COLOR_COUNT = "#F08080";
const COLOR_ALIVE = "#000000";
const COLOR_DEAD = "#FFFFFF";
const COLOR_GRID = "#CCCCCC";

var app = Elm.Main.init({ node: document.querySelector('main') });

function delay(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function getTapeDimensions(width, height, cellSize) {
  let span = Math.floor((width - 1) / (cellSize + 1));
  let depth = Math.floor((height - 1) / (cellSize + 1));

  return { span, depth }
}

const bitIsSet = (n, arr) => {
  const byte = Math.floor(n / 8);
  const mask = 1 << (n % 8);
  return (arr[byte] & mask) === mask;
};

class CellularAutomata {
  constructor(span, ruleIndex, windowSize) {
    this.ruleIndex = ruleIndex;
    this.windowSize = windowSize;
    this.iteration = 0;

    this.universe = Universe.new(span);
    this.rule = BasicRule.new(ruleIndex, windowSize);
    this.state = EmptyState.new();
  }

  setRuleIndex(newRuleIndex) {
    this.ruleIndex = newRuleIndex;

    this.rule = BasicRule.new(newRuleIndex, this.windowSize);
  }

  setWindowSize(newWindowSize) {
    this.windowSize = newWindowSize;

    this.rule = BasicRule.new(this.ruleIndex, newWindowSize);
  }

  setCell(index, active) {
    this.universe.set(index, active);
  }

  clearTape(span) {
    this.iteration = 0;
    this.universe = Universe.new(span);
  }

  get span() {
    return this.universe.span();
  }

  get alive() {
    return this.universe.count_alive();
  }

  tick() {
      this.universe.iterate(this.rule, this.state);
      this.iteration++;
  }
}

customElements.define('cellular-automata',
  class extends HTMLElement {
    constructor() {
      super();

      // DOM
      let ratio = 0.95;

      let canvas = this.querySelector('#ca-canvas');
      canvas.width = ratio * this.offsetWidth;
      canvas.height = this.offsetHeight;

      this.ctx = canvas.getContext('2d');

      let counts = this.querySelector('#ca-counts');
      counts.width = (1 - ratio) * this.offsetWidth;
      counts.height = this.offsetHeight;

      this.counts_ctx = counts.getContext('2d');
      this.counts_max_pixels = counts.width;

      // Grid
      this.cellSize = 10;

      let { span, depth } = getTapeDimensions(canvas.width, canvas.height, this.cellSize);
      this.span = span;
      this.depth = depth;

      // Algorithm
      this.ca = new CellularAutomata(span, BigInt(90), 3);

      // State
      this.animationId = null;
    }

    connectedCallback() {
      this.reset();
    }

    reset() {
      this.ca.clearTape(this.span);
      this.ca.setCell(Math.floor(this.span / 2), true);

      let canvas = this.querySelector('#ca-canvas');
      this.ctx = canvas.getContext('2d');
      this.ctx.clearRect(0, 0, canvas.width, canvas.height);

      let counts = this.querySelector('#ca-counts');
      this.counts_ctx = counts.getContext('2d');
      this.counts_ctx.clearRect(0, 0, counts.width, counts.height);

      drawGrid(this.ctx, this.cellSize, this.ca.span, this.depth);
      drawIteration(this.ctx, this.cellSize, this.depth, this.ca);
      drawStatistics(this.counts_ctx, this.counts_max_pixels, this.cellSize, this.depth, this.ca);
    }

    attributeChangedCallback(name, oldValue, newValue) {
      switch (name) {
        case "rule-index":
          let newRuleIndex = BigInt(newValue);

          this.ca.setRuleIndex(newRuleIndex);
          this.reset();
          break;
          
        case "window-size":
          let newWindowSize = parseInt(newValue, 10);

          this.ca.setWindowSize(newWindowSize);
          this.reset();
          break;

        case "cell-size":
          this.cellSize = parseInt(newValue);

          let canvas = this.querySelector('#ca-canvas');
          let { span, depth } = getTapeDimensions(canvas.width, canvas.height, this.cellSize);
          this.span = span;
          this.depth = depth;

          this.reset();
          break;

        case "desired-state":
          this.changeState(newValue);
          break;
      }
    }

    changeState(state) {
      switch (state) {
        case "pause":
          this.pause();
          break;

        case "resume":
          this.play();
          break;
      }
    }

    static get observedAttributes() {
      return ['rule-index', 'window-size', 'cell-size', 'desired-state'];
    }

    notifyState(state) {
      // build in a delay as not to induce a race condition in Elm, this would cause an update
      // to take place but no view function call to match it
      delay(5)
        .then(() => {
          this.dispatchEvent(new CustomEvent('state-change', {
            bubbles: true,
            detail: {
              state,
            },
          }));
        });
    }

    play() {
      this.renderLoop();
      this.notifyState("running");
    }

    pause() {
      cancelAnimationFrame(this.animationId);
      this.notifyState("idle");
    }

    renderLoop() {
      this.ca.tick();
      drawIteration(this.ctx, this.cellSize, this.depth, this.ca);
      drawStatistics(this.counts_ctx, this.counts_max_pixels, this.cellSize, this.depth, this.ca);

      this.animationId = requestAnimationFrame(this.renderLoop.bind(this));
    }
  }
);

function drawGrid(ctx, cell_size, span, depth) {
  ctx.beginPath();
  ctx.strokeStyle = COLOR_GRID;
  ctx.strokeWidth = 1;

  // vertical lines
  for (let i = 0; i <= span; i++) {
    let x = i * (cell_size + 1) + 1;

    ctx.moveTo(x, 0);
    ctx.lineTo(x, depth * (cell_size + 1));
  }

  // horizontal lines
  for (let i = 0; i <= depth; i++) {
    let y = i * (cell_size + 1) + 1;

    ctx.moveTo(0, y);
    ctx.lineTo(span * (cell_size + 1), y);
  }

  ctx.stroke();
}

function drawIteration(ctx, cell_size, depth, ca) {
  const cellsPtr = ca.universe.as_ptr();
  const cells = new Uint8Array(memory.buffer, cellsPtr, Math.ceil(ca.span / 8));

  ctx.beginPath();

  let layer = ca.iteration % depth;
  let yOffset = layer * (cell_size + 1) + 1;

  for (let c = 0; c <= ca.span; c++) {
    ctx.fillStyle = bitIsSet(c, cells)
      ? COLOR_ALIVE
      : COLOR_DEAD;

    ctx.fillRect(
      c * (cell_size + 1) + 1,
      yOffset,
      cell_size,
      cell_size,
    );
  }

  ctx.stroke();
}

function drawStatistics(ctx, max_pixels, cell_size, depth, ca) {
  ctx.beginPath();

  let layer = ca.iteration % depth;
  let yOffset = layer * (cell_size + 1) + 3;
  let ratioAlive = ca.alive / ca.span;

  ctx.clearRect(0, yOffset, max_pixels, cell_size);
  ctx.fillStyle = COLOR_COUNT;
  ctx.fillRect(0, yOffset, 0.95 * ratioAlive * max_pixels, cell_size);

  ctx.stroke();
}

if ('serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('./sw.js').then(registration => {
      console.log('SW registered: ', registration);
    }).catch(registrationError => {
      console.log('SW registration failed: ', registrationError);
    });
  });
}
