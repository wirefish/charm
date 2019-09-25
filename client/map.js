// A helper object that loads a set of images and runs a callback after each one.
function ImageList(image_urls)
{
    this.image_urls = image_urls;
    this.images = {};
    this.num_loaded = 0;
    this.progress = 0.0;
    this.callback = undefined;

    this.load = function(callback) {
        this.callback = callback;
        for (var i = 0; i < this.image_urls.length; ++i) {
            var [key, url] = this.image_urls[i];
            (function (key, url, self) {
                var image = new Image();
                image.onload = function() { self.onload(this, key); }
                image.src = url;
            })(key, url, this);
        }
    }

    this.onload = function(image, key) {
        this.images[key] = image;
        this.progress = ++this.num_loaded / this.image_urls.length;
        this.callback(this);
    }
}

ImageList.prototype.finishedLoading = function()
{
    return this.num_loaded == this.image_urls.length;
}

// Renders a representation of the rooms around the player.
function Map(canvas)
{
    this.canvas = canvas;
    this.radius = 0;
    this.rooms = undefined;
    this.images = undefined;
}

Map.prototype.domainColors = {
    'indoor': '#484848',
    'underground': '#4f4b32',
}

Map.prototype.surfaceColors = {
    'grass': '#728b2d',
    'weeds': '#9aaa12',
    'flowers': '#f4b2b2',
    'forest': '#3f7741',
    'dirt': '#726020',
    'stone': '#8f8f8f',
    'rocks': '#878470',
    'wood': '#7f7228',
    'sand': '#e2c388',
    'tile': '#f4f0d9',
    'shallow_water': '#6fa1f2',
    'deep_water': '#2e61b2',
}

Map.prototype.defaultSurfaceColor = 'red';

Map.prototype.config = function(images)
{
    this.images = new ImageList(images);
    var this_ = this;
    this.images.load(function(image_list) { this_.onLoadImage(image_list); });
}

Map.prototype.finishedLoading = function()
{
    return this.images != undefined && this.images.finishedLoading();
}

Map.prototype.onLoadImage = function(image_list)
{
    if (this.finishedLoading()) {
        console.log("loaded all map images");
        this.render();
    }
}

Map.prototype.update = function(radius, rooms)
{
    this.radius = radius;

    // Rooms is an array where each item is an array of the following properties:
    // - x offset
    // - y offset
    // - name
    // - icon
    // - quest state
    // - vendor?
    // - exit directions
    // - surface
    // - domain
    this.rooms = rooms;
}

// FIXME: This needs updating.
Map.prototype.showTooltip = function(e)
{
    var cs = Math.min(this.canvas.width, this.canvas.height) / (2 * this.radius + 1);
    var x = e.offsetX / cs;
    var y = e.offsetY / cs;
    var label = "";
    for (var i = 0; i < this.rooms.length; ++i) {
        var r = this.rooms[i];
        if (x >= r.x && x < r.x + 1 && y >= r.y && y < r.y + 1) {
            label = r.name + " " + r.terrain + " " + r.x + " " + r.y;
            break;
        }
    }
}

Map.prototype.drawTexture = function(context, i, j, texture_id, size)
{
    var image = this.images.images[texture_id];
    context.drawImage(image, 64, 64, 128, 128, 0, 0, size + 1, size + 1);
}

var exit_lines = {
    'north': [0, -1],
    'northeast': [1, -1],
    'east': [1, 0],
    'southeast': [1, 1],
    'south': [0, 1],
    'southwest': [-1, 1],
    'west': [-1, 0],
    'northwest': [-1, -1],
};

Map.prototype.drawSymbol = function(context, symbol, left, top, size)
{
    var radius = size / 2;
    var cx = left + radius;
    var cy = top + radius;

    context.font = size + "px arial";
    context.lineWidth = 1;
    context.strokeStyle = "black";
    context.fillText(symbol, cx, cy);
    context.strokeText(symbol, cx, cy);
}

Map.prototype.drawUpDown = function(context, inset, room_size)
{
    context.fillStyle = 'black';
    context.beginPath();
    context.moveTo(inset + 4, inset);
    context.lineTo(inset + room_size / 3, inset);
    context.lineTo(inset + room_size / 6 + 2, inset - room_size / 6);
    context.lineTo(inset + 4, inset);
    context.fill();
    context.stroke();
}

Map.prototype.render = function()
{
    if (!this.finishedLoading() || !this.rooms)
        return;

    var context = this.canvas.getContext("2d");
    context.imageSmoothingQuality = "high";

    context.fillStyle = "#2a2a2e";
    context.fillRect(0, 0, this.canvas.width, this.canvas.height);

    var diameter = 2 * this.radius + 1;
    var cell_size = Math.min(this.canvas.width, this.canvas.height) / diameter;
    var left = (this.canvas.width - diameter * cell_size) / 2;
    var top = (this.canvas.height - diameter * cell_size) / 2;
    var inset = cell_size / 5;
    var room_size = cell_size - inset * 2;

    context.textAlign = 'center';
    context.textBaseline = 'middle';

    // Fill the backgrounds for the rooms based on their domain.
    context.lineWidth = 16;
    context.lineCap = 'butt';
    for (var j = 0; j < this.rooms.length; ++j) {
        var [x, y, name, icon, quest_state, vendor, exits, surface, domain] = this.rooms[j];

        context.save();
        context.translate(left + (x + this.radius) * cell_size,
                          top + (y + this.radius) * cell_size);

        // Fill the cell.
        var c = this.domainColors[domain];
        if (c != undefined) {
            context.beginPath();
            context.fillStyle = c;
            context.rect(-1, -1, cell_size + 2, cell_size + 2);
            context.fill();
        }

        // For an underground room, draw a wider line along the exit.
        if (domain == 'underground') {
            context.strokeStyle = c;
            context.beginPath();
            for (var i = 0; i < exits.length; ++i) {
                var line = exit_lines[exits[i]];
                if (line) {
                    var [lx, ly] = line;
                    var sx = cell_size / 2 + room_size / 2 * lx;
                    var sy = cell_size / 2 + room_size / 2 * ly;
                    var ex = cell_size / 2 * (1 + lx) + lx;
                    var ey = cell_size / 2 * (1 + ly) + ly;
                    context.moveTo(sx, sy);
                    context.lineTo(ex, ey);
                }
            }
            context.stroke();
        }

        context.restore();
    }

    // Render the foreground of each room.
    context.lineWidth = 4;
    context.lineCap = 'round';
    context.strokeStyle = 'black';
    for (var j = 0; j < this.rooms.length; ++j) {
        var [x, y, name, icon, quest_state, vendor, exits, surface, domain] = this.rooms[j];

        context.save();
        context.translate(left + (x + this.radius) * cell_size,
                          top + (y + this.radius) * cell_size);

        // Render a highlight around the current room.
        if (x == 0 && y == 0) {
            context.beginPath();
            var border_size = room_size * 1.33;
            var border_inset = (cell_size - border_size) / 2;
            context.rect(border_inset, border_inset, border_size, border_size);
            context.fillStyle = '#c0c080';
            context.fill();
        }

        // Fill the room interior based on the surface.
        var surface_color = this.surfaceColors[surface];
        if (!surface_color)
            surface_color = this.defaultSurfaceColor;
        context.beginPath();
        context.fillStyle = surface_color;
        context.rect(inset, inset, room_size, room_size);
        context.fill();

        // Draw an icon if one is defined.
        if (icon) {
            var [icon_name, icon_color] = icon;
            var image = this.images.images[icon_name];
            context.drawImage(image, inset + 2, inset + 2, room_size - 4, room_size - 4);
        }

        // Draw the room border and lines for exits.
        context.beginPath();
        context.rect(inset, inset, room_size, room_size);
        for (var i = 0; i < exits.length; ++i) {
            var line = exit_lines[exits[i]];
            if (line) {
                var [lx, ly] = line;
                var sx = cell_size / 2 + room_size / 2 * lx;
                var sy = cell_size / 2 + room_size / 2 * ly;
                var ex = cell_size / 2 * (1 + lx);
                var ey = cell_size / 2 * (1 + ly);
                context.moveTo(sx, sy);
                context.lineTo(ex, ey);
            }
        }
        context.stroke();

        // Draw the up and down indicators if needed.
        if (exits.indexOf('up') != -1) {
            this.drawUpDown(context, inset, room_size);
        }
        if (exits.indexOf('down') != -1) {
            context.save();
            context.translate(cell_size, cell_size);
            context.scale(-1, -1);
            this.drawUpDown(context, inset, room_size);
            context.restore();
        }

        // Draw a symbol to denote quest state.
        if (quest_state) {
            var heart = "\u2665";
            if (quest_state == "complete")
                context.fillStyle = "#efcf3f";  // gold
            else if (quest_state == "incomplete")
                context.fillStyle = "#afafbf";  // silver
            else // "available"
                context.fillStyle = "#1565c1";  // blue
            this.drawSymbol(context, heart, inset, inset, room_size / 2 + 2);
        }

        // Draw a symbol if there's a vendor.
        if (vendor) {
            context.fillStyle = "#efcf3f";  // gold
            this.drawSymbol(context, "\u2666",
                            inset + room_size / 2 - 2, inset, room_size / 2 + 2)
        }

        context.restore();
    }

    // var this_ = this;
    // canvas.onmousemove = function(e) { this_.showTooltip(e); }
}
