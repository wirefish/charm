'use strict';

// Images used as icons within map cells.
var icon_urls = {
    'ship': 'icons/map_boat.png',
    'house': 'icons/map_house.png',
    'quest_available': 'icons/map_quest_available.png',
    'quest_incomplete': 'icons/map_quest_incomplete.png',
    'quest_complete': 'icons/map_quest_complete.png',
    'vendor': 'icons/map_vendor.png',
    'trainer': 'icons/map_trainer.png',
};

// Images used as patterns for drawing surfaces and the outlines of indoor and
// undergroup locations.
var pattern_urls = {
    'grass': 'images/grass.png',
    'forest': 'images/forest.png',
    'dirt': 'images/dirt.jpg',
    'wood': 'images/wood.png',
    'stone': 'images/cobblestone.png',
    'rocks': 'images/rubble.png',
    'shallow_water': 'images/water.png',
    'underground': 'images/underground.png',
    'indoor': 'images/indoor.png',
};

// Loads a set of images and runs a callback after each one.

function ImageLoader(image_urls)
{
    this.image_urls = image_urls;
    this.images = {};
    this.finish_callback = undefined;
}

ImageLoader.prototype.loadImages = function(callback)
{
    this.callback = callback;

    for (var key in this.image_urls) {
        var url = this.image_urls[key];
        (function (key, url, loader) {
            var image = new Image();
            image.onload = function() { loader.onLoadImage(key, this); }
            image.src = url;
        })(key, url, this);
    }
}

ImageLoader.prototype.onLoadImage = function(key, image)
{
    this.images[key] = image;
    this.callback(this);
}

ImageLoader.prototype.finishedLoading = function()
{
    return Object.keys(this.images).length == Object.keys(this.image_urls).length;
}

// Renders a representation of the rooms around the player.

function Map(canvas)
{
    this.canvas = canvas;
    this.radius = 0;
    this.rooms = undefined;

    var self = this;

    this.icons = undefined;
    var icon_loader = new ImageLoader(icon_urls);
    icon_loader.loadImages(function(loader) { self.onLoadIcon(loader); });

    this.patterns = undefined;
    var pattern_loader = new ImageLoader(pattern_urls);
    pattern_loader.loadImages(function(loader) { self.onLoadPattern(loader); });
}

Map.prototype.onLoadIcon = function(loader)
{
    if (loader.finishedLoading()) {
        this.icons = loader.images;
    }
}

Map.prototype.onLoadPattern = function(loader)
{
    if (loader.finishedLoading()) {
        // Convert the images into patterns.
        var context = this.canvas.getContext('2d');
        this.patterns = {};
        for (var key in loader.images) {
            var image = loader.images[key];
            this.patterns[key] = context.createPattern(image, 'repeat');
        }
    }
}

Map.prototype.finishedLoading = function()
{
    return this.icons != undefined && this.patterns != undefined;
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
    // - trainer?
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

Map.prototype.resize = function()
{
    this.canvas.width = this.canvas.clientWidth;
    this.canvas.height = this.canvas.clientHeight;
    this.render();
}

Map.prototype.render = function()
{
    if (!this.finishedLoading() || !this.rooms)
        return;

    var context = this.canvas.getContext("2d");
    context.imageSmoothingQuality = "high";
    context.fillStyle = "#2a2a2e";
    context.fillRect(0, 0, this.canvas.width, this.canvas.height);

    // Figure out the size and placement of the map cells. Each cell is square
    // and has a size that is a multiple of four, as this reduces rendering
    // artifacts due to fractional pixel coordinates.
    var map_size = Math.max(this.canvas.width, this.canvas.height);
    var diameter = 2 * this.radius + 1;
    var cell_size = (Math.ceil(map_size / diameter) + 3) & ~3;
    var center_left = Math.floor((this.canvas.width - cell_size) / 2);
    var center_top = Math.floor((this.canvas.height - cell_size) / 2);
    var inset = cell_size / 4;
    var room_size = cell_size - inset * 2;

    for (var j = 0; j < this.rooms.length; ++j) {
        var [x, y, name, icon, quest_state, vendor, trainer, exits,
             surface, surrounding, domain] = this.rooms[j];

        var left = center_left + x * cell_size, top = center_top + y * cell_size;

        // Fill the cell background.
        if (domain == 'outdoor') {
            var pattern = this.patterns[surrounding ? surrounding : surface];
            if (pattern) {
                context.fillStyle = pattern;
                context.fillRect(left, top, cell_size, cell_size);
            }
        }
        else {
            var pattern = this.patterns[domain];
            if (pattern) {
                context.fillStyle = pattern;
                context.fillRect(left, top, cell_size, cell_size);
            }
        }

        // For an underground room, draw a wider line along each exit.
        if (domain == 'underground') {
            context.strokeStyle = this.patterns[domain];
            context.lineWidth = cell_size / 4;
            context.lineCap = 'butt';
            context.beginPath();
            for (var i = 0; i < exits.length; ++i) {
                var line = exit_lines[exits[i]];
                if (line) {
                    var [lx, ly] = line;
                    var sx = cell_size / 2 + room_size / 2 * lx;
                    var sy = cell_size / 2 + room_size / 2 * ly;
                    var ex = cell_size / 2 * (1 + lx) + lx;
                    var ey = cell_size / 2 * (1 + ly) + ly;
                    context.moveTo(left + sx, top + sy);
                    context.lineTo(left + ex, top + ey);
                }
            }
            context.stroke();
        }

        // Render a highlight around the current room.
        if (x == 0 && y == 0) {
            context.lineWidth = 8;
            context.strokeStyle = '#c8c868';
            context.strokeRect(left + inset - 4, top + inset - 4,
                               room_size + 8, room_size + 8);
        }

        // Render the foreground of each room.
        context.lineWidth = 4;
        context.lineCap = 'round';

        // Fill the room interior based on the surface.
        if (domain != 'outdoor' || surrounding) {
            var pattern = this.patterns[surface];
            if (pattern) {
                context.fillStyle = pattern;
                context.fillRect(left + inset, top + inset, room_size, room_size);
            }
        }

        // Lighten the room interior to make markers easier to see.
        context.globalCompositeOperation = 'screen';
        context.beginPath();
        context.fillStyle = '#4f4f4f';
        context.rect(left + inset, top + inset, room_size, room_size);
        context.fill();
        context.globalCompositeOperation = 'source-over';

        // Draw an icon if one is defined.
        if (icon) {
            var image = this.icons[icon];
            if (image)
                context.drawImage(image, left + inset + 2, top + inset + 2,
                                  room_size - 4, room_size - 4);
        }

        // Draw the room border and lines for exits.
        context.strokeStyle = '#1f1f1f';
        context.strokeRect(left + inset, top + inset, room_size, room_size);
        if (exits) {
            context.beginPath();
            for (var i = 0; i < exits.length; ++i) {
                var line = exit_lines[exits[i]];
                if (line) {
                    var [lx, ly] = line;
                    var sx = cell_size / 2 + room_size / 2 * lx;
                    var sy = cell_size / 2 + room_size / 2 * ly;
                    var ex = cell_size / 2 * (1 + lx);
                    var ey = cell_size / 2 * (1 + ly);
                    context.moveTo(left + sx, top + sy);
                    context.lineTo(left + ex, top + ey);
                }
            }
            context.stroke();

            context.fillStyle = '#1f1f1f';
            if (exits.indexOf('up') != -1) {
                // Upward-pointing triangle in upper-left corner.
                var l = left + inset + 2, r = left + cell_size / 2 - 2,
                    b = top + inset, t = top + inset / 2;
                context.beginPath();
                context.moveTo(l, b);
                context.lineTo(r, b);
                context.lineTo((l + r) / 2, t);
                context.closePath();
                context.fill();
            }
            if (exits.indexOf('down') != -1) {
                // Downward-pointing triangle in lower-right corner.
                var l = left + cell_size / 2 + 2, r = left + cell_size - inset - 2,
                    t = top + cell_size - inset, b = top + cell_size - inset / 2;
                context.beginPath();
                context.moveTo(l, t);
                context.lineTo((l + r) / 2, b);
                context.lineTo(r, t);
                context.closePath();
                context.fill();
            }
        }

        // Draw a symbol to denote quest state.
        if (quest_state) {
            var image = this.icons["quest_" + quest_state];
            context.drawImage(image, left + inset + 2, top + inset + 2,
                              room_size / 2 - 2, room_size / 2 - 2);
        }

        // Draw a symbol if there's a vendor.
        if (vendor) {
            var image = this.icons["vendor"];
            context.drawImage(image, left + inset + room_size / 2, top + inset + 2,
                              room_size / 2 - 2, room_size / 2 - 2);
        }

        // Draw a symbol if there's a trainer.
        if (trainer) {
            var image = this.icons["trainer"];
            context.drawImage(image, left + inset + 2, top + inset + room_size / 2,
                              room_size / 2 - 2, room_size / 2 - 2);
        }
    }
}
