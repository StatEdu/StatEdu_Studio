"""Check the browser-captured split plot snapshot, independently of export files."""
import base64
import io
import json
import re
from pathlib import Path
from PIL import Image
import numpy as np

entry = json.loads(Path('tmp/split-regression-localized/entries.json').read_text(encoding='utf-8'))[0]
images = re.findall(r'src="data:image/[^;]+;base64,([^"]+)"', entry['html'])
assert len(images) == 4, f'Expected two diagnostic plots per group, got {len(images)}'
for index, encoded in enumerate(images, 1):
    image = Image.open(io.BytesIO(base64.b64decode(encoded))).convert('RGB')
    assert image.width > 100 and image.height > 100
    # These fixed regression fixtures use slate-blue observation markers. Count
    # those inside the plotting area, excluding axes/text and the bright blue smoother.
    pixels = np.asarray(image, dtype=np.int16)[30:340, 84:392]
    red, green, blue = pixels[:,:,0], pixels[:,:,1], pixels[:,:,2]
    markers = (red > 40) & (red < 130) & (green > red + 3) & (blue > green + 3) & (blue < 180)
    marker_pixels = int(markers.sum())
    assert marker_pixels > 100, f'Plot {index} lacks observation markers: {marker_pixels}'
    print(f'PASS: group {(index + 1) // 2}, plot {(index - 1) % 2 + 1}: {image.width}x{image.height}, {marker_pixels} observation-marker pixels')
