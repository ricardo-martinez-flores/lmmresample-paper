# Diagrams

Figures 1 and 8 of the article are diagrams, built as editable PowerPoint files.

| File | Figure |
|---|---|
| `figure1_concept.pptx` | Figure 1: how lmmresample resamples whole trials and participants |
| `figure1_workflow.pptx` | Figure 8: the problem and the six-step workflow |

They can be edited directly. To rebuild them, draw the small panels with
`python3 minis.py` and `python3 minis_a.py` (matplotlib), which write the
images in `img/`, and then run `node build.js` (pptxgenjs).
