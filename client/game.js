'use strict';

var max_blocks = 500;

var command_history = new Array(100);
var command_pos = 0;

var ws = null;
var map = null;
var handler = null;

//
// Resize and rearrange all UI elements as needed for the current window size.
//
function layout()
{
    var margin = 4;  // between all top-level boxes

    var window_width = window.innerWidth;
    var window_height = window.innerHeight;

    // Determine the desired size of the map, since this drives most other size
    // decisions.
    var map_size = Math.min(Math.round(window_width * 0.375), Math.round(window_height * 0.5));

    var input = document.getElementById("input");
    var input_height = input.offsetHeight;

    var vitals = document.getElementById("vitals");
    var vitals_height = vitals.offsetHeight;
    vitals.style.left = vitals.style.top = margin;
    vitals.style.width = window_width - map_size - 3 * margin;

    var player_bars = document.getElementById("player_bars");
    player_bars.style.width = (window_width - map_size - 4 * margin - 80);

    var contents = document.getElementById("contents");
    var contents_width = contents.offsetWidth;
    contents.style.left = margin;
    contents.style.top = vitals_height + 2 * margin;
    contents.style.height = window_height - (vitals_height + input_height + 4 * margin);

    var buttons = document.getElementById("buttons");
    buttons.style.left = margin;
    buttons.style.top = window_height - (input_height + margin);
    buttons.style.height = input_height;

    var game_text = document.getElementById("game_text");
    game_text.style.top = vitals_height + 2 * margin;
    game_text.style.left = contents_width + 2 * margin;
    game_text.style.width = window_width - (contents_width + map_size + 4 * margin);
    game_text.style.height = window_height - (vitals_height + input_height + 4 * margin);

    input.style.left = contents_width + 2 * margin;
    input.style.top = window_height - (input_height + margin);
    input.style.width = window_width - (contents_width + map_size + 4 * margin);

    var map = document.getElementById("map");
    map.style.left = window_width - map_size - margin;
    map.style.top = margin;
    map.style.width = map.style.height = map_size;

    var map_canvas = document.getElementById("map_canvas");
    var top_margin = 44;  // FIXME: from height of room_name and zone_name
    var canvas_size = map_size - (top_margin + margin);
    map_canvas.style.left = (map_size - canvas_size) / 2;
    map_canvas.style.top = top_margin;
    map_canvas.width = map_canvas.height = canvas_size;

    var chat = document.getElementById("chat_text");
    chat.style.left = window_width - (map_size + margin);
    chat.style.top = map_size + 2 * margin;
    chat.style.width = map_size;
    chat.style.height = window_height - (map_size + input_height + 4 * margin);

    // The offset by 4 is to make the button margins look right.
    var macros = document.getElementById("macros");
    macros.style.left = window_width - (map_size + margin) - 4;
    macros.style.top = window_height - (input_height + margin);
    macros.style.width = map_size + 4;
    macros.style.height = input_height;
}

function resize()
{
    layout();
    map.render();
}
window.onresize = resize;

function load()
{
    layout();

    map = new Map(document.getElementById("map_canvas"));
    map.config([
        ["ship", "icons/sailboat.png"],
        ["house", "icons/transparent_house.png"],
        ["hearts", "icons/hearts.png"],
    ]);
    map.render();

    handler = new MessageHandler();
}
window.onload = load;

//
// Give the input focus whenever the user hits Return inside the window.
//
function keypress(e) {
    var input = document.getElementById("command");
    if (document.activeElement != input && String.fromCharCode(e.which) == "\r") {
        document.getElementById("command").focus();
        return false;
    }
}
window.onkeypress = keypress;

//
// Add some useful String methods.
//

String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}

String.prototype.format = function() {
    var args = arguments;
    return this.replace(/{(\d+)}/g, function(match, number) {
        return typeof args[number] != 'undefined' ? args[number] : match;
    });
}

function defined(val)
{
    return val != undefined;
}

function link(s, type)
{
    if (type)
        return '`{0}:{1}`'.format(type, s);
    else
        return '`{0}`'.format(s);
}

function look(s)
{
    return link(s, 'look');
}

function setIcon(element, icon)
{
    if (icon) {
        element.style.backgroundImage = "url('icons/{0}.png')".format(icon);
    }
}

//
// Updates a stat bar to reflect a current and maximum value.
//
function updateBar(id, current, max)
{
    var p = max ? Math.min(100, 100.0 * current / max) : 0;
    var bar = document.getElementById(id);
    bar.children[0].style.width = p + "%";
    bar.children[1].innerHTML = current + "&thinsp;/&thinsp;" + max;
}

function updatePlayerBio(name, icon, level, race)
{
    var summary = name ?
        '{0}, level {1} {2}'.format(name, level, race) :
        'level {0} {1}'.format(level, race);
    document.getElementById("player_name").innerHTML = summary;

    setIcon(document.getElementById("player_portrait"), icon);
}

// Appends a block element to a scrollable text pane, removing the oldest block
// first if the maximum number of blocks would be exceeded.
function appendBlock(block, containerId = 'game_text')
{
    var container = document.getElementById(containerId);
    if (container.childNodes.length >= max_blocks)
        container.removeChild(container.firstChild);
    container.appendChild(block);
    container.scrollTop = container.scrollHeight;
}

var PANES = {'social': 'chatbox',
             'equipment': 'equipbox',
             'inventory': 'invbox',
             'modifiers': 'combatbox',
             'skills': 'skillbox'};

// An object that encapsulates functions callable based on messages from the
// server.
function MessageHandler()
{
    // The cached properties for the player's avatar.
    this.avatar = {};

    // The cached equipment for the avatar is a map from slot name to a brief
    // item description.
    this.equipment = {};

    // Enemies that are currently in combat with the player.
    this.enemies = new Set();

    // The enemy that is targeted by player attacks.
    this.enemy_target = undefined;

    // True to show paths, etc. for debugging.
    this.debug = true;

    // Select the social chat pane by default.
    this.currentPane = 'social';
    document.getElementById(this.currentPane).style.opacity = '1.0';
}

MessageHandler.prototype.showPane = function(button_id)
{
    document.getElementById(this.currentPane).style.opacity = '0.5';
    document.getElementById(PANES[this.currentPane]).style.display = 'none';

    this.currentPane = button_id;

    document.getElementById(this.currentPane).style.opacity = '1.0';
    document.getElementById(PANES[this.currentPane]).style.display = 'block';
}

MessageHandler.prototype.showRaw = function(text)
{
    var element = document.createElement("pre");
    element.innerHTML = text;
    appendBlock(element);
}

// Display game text in the style defined by the given class.
MessageHandler.prototype.showText = function(text, className)
{
    appendBlock(wrapElements('div', formatText(text), className));
}

MessageHandler.prototype.showNotice = function(text)
{
    this.showText(text, 'notice');
}

MessageHandler.prototype.showTutorial = function(text)
{
    this.showText(text, 'tutorial');
}

MessageHandler.prototype.showError = function(text)
{
    this.showText(text, 'error');
}

MessageHandler.prototype.updateMap = function(room_name, zone_name, radius, rooms)
{
    document.getElementById("room_name").innerHTML = formatBlock(room_name);
    document.getElementById("zone_name").innerHTML = zone_name;
    map.update(radius, rooms);
    map.render();
}

MessageHandler.prototype.updateAvatar = function(properties)
{
    // Update the cached properties.
    this.avatar = Object.assign({}, this.avatar, properties);

    // Update UI elements that have changed.
    if (properties.name || properties.icon || properties.level || properties.race)
        updatePlayerBio(this.avatar.name, this.avatar.icon,
                        this.avatar.level, this.avatar.race);
    if (properties.health || properties.max_health)
        updateBar('player_health', this.avatar.health, this.avatar.max_health);
    if (properties.energy || properties.max_energy)
        updateBar('player_energy', this.avatar.energy, this.avatar.max_energy);
    if (properties.mana || properties.max_mana)
        updateBar('player_mana', this.avatar.mana, this.avatar.max_mana);
    if (properties.xp || properties.xp_required)
        updateBar('player_xp', this.avatar.xp, this.avatar.xp_required);
}

MessageHandler.prototype.updateAuras = function(key, auras)
{
    var parent = undefined;
    if (key == this.player_path)
        parent = document.getElementById('vitals');
    else
        parent = document.getElementById(getNeighborId(path));
    var aurabox = parent.getElementsByClassName('aurabox')[0];

    if (aurabox) {
        while (aurabox.firstChild)
            aurabox.removeChild(aurabox.firstChild);

        for (var i = 0; i < auras.length; ++i) {
            var [name, icon, color, end_time] = auras[i];
            var item = document.createElement('div');
            item.className = 'aura';
            item.style.backgroundImage = "url('icons/{0}')".format(icon);
            item.style.backgroundColor = color;
            aurabox.appendChild(item);
        }
    }
}

function formatModifiers(modifiers)
{
    var parts = [];
    for (let name in modifiers) {
        var value = modifiers[name];
        var sign = value >= 0 ? '+' : '&minus;';
        parts.push('{0} {1}{2}'.format(name, sign, Math.round(value)))
    }
    if (parts.length == 0)
        return 'none';
    else
        return parts.join(', ');
}

MessageHandler.prototype.showStatus = function(stats)
{
    var lines = [];

    if (stats.name) {
        lines.push('You are {0}, a level {1} {2}.'.format(
            stats.name, stats.level, removeArticle(stats.race)));
    }
    else {
        lines.push('You a level {1} {2}.'.format(
            stats.level, removeArticle(stats.race)));
    }

    /*
    for (var i = 0; i < stats.guilds.length; ++i) {
        var [profession, guild, rank] = stats.guilds[i];
        lines.push('You are a rank {0} {1} in {2}.'.format(
            rank, removeArticle(profession), guild));
    }

    lines.push('You have {0} unspent karma points.'.format(stats.karma));

    var attributes = [];
    for (let name of ATTRIBUTE_NAMES)
        attributes.push('{0} {1}'.format(name, Math.round(stats.modifiers[name])));
    lines.push('Attributes: ' + attributes.join(', '));

    var attack_modifiers = {};
    var defense_modifiers = {};
    var proficiencies = {};
    var other = {};
    for (let key in stats.modifiers) {
        var value = stats.modifiers[key];
        var sep = key.indexOf('_');
        if (sep >= 0) {
            var [prefix, suffix] = key.split('_', 2)
            if (suffix == 'damage')
                attack_modifiers[prefix] = value;
            else if (suffix == 'resistance')
                defense_modifiers[prefix] = value;
            else if (suffix == 'proficiency')
                proficiencies[prefix] = value;
            else
                other[prefix] = value;
        }
    }

    lines.push('Attack modifiers: ' + formatModifiers(attack_modifiers));
    lines.push('Defense modifiers: ' + formatModifiers(defense_modifiers));
    lines.push('Proficiencies: ' + formatModifiers(proficiencies));
    */

    appendBlock(wrapElements(
        'div', lines.map(function (line) { return makeTextElement('p', line); })));
}

function joinClauses(clauses) {
    if (clauses.length == 1)
        return clauses[0];
    else
        return clauses.slice(0, -1).join(', ') + ' and ' + clauses[clauses.length - 1];
}

MessageHandler.prototype.updateInventory = function(inventory)
{
    var invbox = document.getElementById('invbox');
    while (invbox.firstChild)
        invbox.removeChild(invbox.firstChild);

    var groups = Array.from(Object.keys(inventory)).sort();
    for (let group of groups) {
        var heading = document.createElement('h3');
        heading.innerHTML = group;
        invbox.appendChild(heading);

        var items = inventory[group];
        for (var i = 0; i < items.length; ++i) {
            var [brief, icon, color] = items[i];
            var entry = document.createElement('div');

            var icon_div = document.createElement('div');
            icon_div.style.backgroundImage = 'url("icons/{0}")'.format(icon);
            icon_div.style.backgroundColor = color;
            entry.appendChild(icon_div);

            var name_div = document.createElement('div');
            name_div.innerHTML = formatBlock(look(brief));
            entry.appendChild(name_div);

            invbox.appendChild(entry);
        }
    }
}

MessageHandler.prototype.updateEquipment = function(equipment)
{
    // Update the cached equipment.
    this.equipment = Object.assign({}, this.equipment, equipment);

    // Update the UI elements.
    for (var slot in equipment) {
        var div = document.getElementById('equip_' + slot);
        var item = equipment[slot];
        if (item) {
            var [icon, brief] = item;
            div.children[0].style.backgroundImage = 'url("icons/{0}.png")'.format(icon);
            div.children[0].style.visibility = 'visible';
            div.children[1].innerHTML = brief;
        }
        else {
            div.children[0].style.visibility = 'hidden';
            div.children[1].innerHTML = null;
        }
    }
}

MessageHandler.prototype.showInventory = function(inventory, equipment)
{
    var out = [];

    this.updateInventory(inventory);

    var weapons = [null, null];
    var attire = [];
    for (var i = 0; i < equipment.length; ++i) {
        var [slot, item] = equipment[i];
        if (slot == 'main_hand')
            weapons[0] = item;
        else if (slot == 'off_hand')
            weapons[1] = item;
        else if (slot == 'both_hands')
            weapons[0] = weapons[1] = item;
        else
            attire.push('{0} ({1})'.format(look(item), slot));
    }

    if (weapons[0]) {
        if (weapons[0] == weapons[1])
            out.push('You are wielding {0} in both hands.'.format(look(weapons[0])));
        else if (weapons[1])
            out.push('You are wielding {0} in your main hand and {1} in your off hand.'.format(
                look(weapons[0]), look(weapons[1])));
        else
            out.push('You are wielding {0} in your main hand.'.format(look(weapons[0])));
    }
    else if (weapons[1]) {
        out.push('You are wielding {0} in your off hand.'.format(look(weapons[1])));
    }
    else {
        out.push('You are not wielding any weapons.');
    }

    if (attire.length == 0) {
        out.push('You are not wearing anything.');
    }
    else {
        out.push('You are wearing ' + joinClauses(attire) + '.');
    }

    if (inventory.length == 0) {
        out.push('You are not carrying anything.');
    }
    else {
        var list_items = [];
        for (let i = 0; i < inventory.length; ++i) {
            var [group, items] = inventory[i];
            var links = items.map(function (item) { return look(item[0]); });
            list_items.push(group + ': ' + joinClauses(links));
        }
        out.push('You are carrying:');
        out.push(makeList(list_items));
    }

    appendBlock(wrapElements('div', formatStrings('p', out)));
}

MessageHandler.prototype.startPlayerCast = function(name, duration)
{
    var bar = document.getElementById("player_mana");

    bar.children[1].style.animation = "";
    bar.children[0].innerHTML = name;
    window.requestAnimationFrame(function (t) {
        window.requestAnimationFrame(function (t) {
            bar.children[1].style.animation = "cast {0} linear".format(duration);
        });
    });
}

MessageHandler.prototype.cancelPlayerCast = function(mana, max_mana)
{
    var bar = document.getElementById("player_mana");
    bar.children[1].style = "";
    updateBar('player_mana', mana, max_mana)
}

function getNeighborId(key)
{
    return 'neighbor_' + key;
}

function removeNeighborHighlight(key)
{
    var element = document.getElementById(getNeighborId(key));
    if (element) {
        var portrait = element.children[0];
        portrait.className = portrait.className.split(' ').filter(
            function (c) { return !c.startsWith('highlight_'); }).join(' ');
    }
}

function setNeighborHighlight(key, type)
{
    var element = document.getElementById(getNeighborId(key));
    if (element) {
        var portrait = element.children[0];
        var classes = portrait.className.split(' ').filter(
            function (c) { return !c.startsWith('highlight_'); });
        classes.push('highlight_' + type);
        portrait.className = classes.join(' ');
    }
}

MessageHandler.prototype.setNeighborProperties = function(item, properties)
{
    item.id = getNeighborId(properties.key);

    var old_properties = item.getAttribute("charm-properties");
    var new_properties = old_properties ?
        Object.assign({}, old_properties, properties) :
        properties;
    item.setAttribute("charm-properties", new_properties);

    // children of item are, in order: portrait, name, healthbar, manabar, aurabox

    if (properties.icon)
        setIcon(item.children[0], properties.icon);

    if (properties.state) {
        item.children[0].className = 'portrait highlight_' + properties.state;
    }

    if (properties.brief)
        item.children[1].innerHTML = properties.brief;

    if (defined(properties.health) || properties.max_health) {
        item.children[2].children[0].style.width =
            (100.0 * properties.health / properties.max_health) + "%";
    }
    else {
        item.children[2].style.visibility = 'hidden';
    }

    if (defined(properties.mana) || properties.max_mana) {
        item.children[3].children[0].style.width =
            (100.0 * properties.mana / properties.max_mana) + "%";
    }
    else {
        item.children[3].style.visibility = 'hidden';
    }

    // TODO: auras

    // Set a command to perform when clicking the item. TODO: make it
    // appropriate, or add a popup with a few options.
    item._command = 'look {0} #{1}'.format(properties.brief, properties.key);
    item.onmousedown = function() { sendInput(this._command); }
}

MessageHandler.prototype.createNeighbor = function(properties)
{
    var contents = document.getElementById("contents");
    var item = contents.children[0].cloneNode(true);
    this.setNeighborProperties(item, properties);
    item.className = "do_enter";
    item.style.display = "block";
    contents.appendChild(item);
    return item;
}

MessageHandler.prototype.setNeighbors = function(neighbors)
{
    var contents = document.getElementById("contents");

    // Remove all but the first child, which is the invisible prototype used to
    // instantiate other items.
    while (contents.children[0].nextSibling)
        contents.removeChild(contents.children[0].nextSibling);

    // Add each neighbor.
    if (neighbors) {
        for (var i = 0; i < neighbors.length; ++i)
            this.createNeighbor(neighbors[i]);
    }
}

MessageHandler.prototype.updateNeighbor = function(properties, message)
{
    var item = document.getElementById(getNeighborId(properties.key));
    if (item)
        this.setNeighborProperties(item, properties);
    else
        this.createNeighbor(properties);

    if (message)
        this.showText(message);
}

MessageHandler.prototype.removeNeighbor = function(key, exit_pose)
{
    var item = document.getElementById(getNeighborId(key));

    if (exit_pose) {
        var properties = item.getAttribute("charm-properties");
        this.showText("{0} {1}".format(properties.brief.capitalize(), exit_pose));
    }

    item.addEventListener('animationend', function (event) {
        this.parentNode.removeChild(this);
    });
    item.className = "";
    window.requestAnimationFrame(function (t) {
        window.requestAnimationFrame(function (t) {
            item.className = "do_exit";
        });
    });
}

// Called when an entity `actor` takes another entity `target`, removing it from
// the current location and possibly replacing it with a new entity
// `replacement` (as when taking fewer than an entire stack of items).
MessageHandler.prototype.didTake = function(actor_path, target_path, count, replacement)
{
    var target_name = this.neighbors[target_path].brief;

    if (actor_path == this.player_path) {
        var msg = 'You take {0}.'.format(target_name);
    }
    else {
        var msg = '{0} takes {1}.'.format(
            this.neighbors[actor_path].brief.capitalize(), target_name);
    }
    this.showText(msg);

    if (replacement != undefined)
        this.replaceNeighbor(target_path, replacement);
    else
        this.removeNeighbor(target_path);
}

// Called when an entity `actor` drops another entity `target`, adding it to the
// current location and possibly replacing a previous entity `replace_path` (as
// adding to a stack of items).
MessageHandler.prototype.didDrop = function(actor_path, target, count, replace_path)
{
    if (actor_path == this.player_path) {
        var msg = 'You drop {0}.'.format(target.brief);
    }
    else {
        var msg = '{0} takes {1}.'.format(
            this.neighbors[actor_path].brief.capitalize(), target.brief);
    }
    this.showText(msg);

    if (replace_path != undefined)
        this.replaceNeighbor(replace_path, target.id);
    else
        this.addNeighbor(target);
}

// Called when an entity `actor` gives the player an entity representing `count`
// items described by `brief`.
MessageHandler.prototype.didGive = function(actor_path, count, brief)
{
    var actor_brief = this.neighbors[actor_path].brief.capitalize();

    var item;
    if (count == 1)
        item = brief;
    else
        item = makePlural(brief, count);

    var msg = '{0} gives you {1}.'.format(actor_brief, look(item));
    this.showText(msg);
}

MessageHandler.prototype.formatEmote = function(key, brief, pose)
{
    var s = '{0} {1}'.format(look(brief.capitalize()), pose);
    if (this.debug)
        s += ' (#{0})'.format(key);
    return makeTextElement('p', s);
}

MessageHandler.prototype.showEmote = function(path, count, brief, pose)
{
    appendBlock(wrapElements('div', [this.formatEmote(path, count, brief, pose)]));
}

MessageHandler.prototype.showHelp = function(text)
{
    this.showText(text, 'help');
}

MessageHandler.prototype.showLinks = function(heading, prefix, topics)
{
    var elements = [];

    elements.push(makeTextElement('p', heading));
    elements.push(wrapElements('p', topics.map(function (topic) {
        var link = makeTextElement('span', topic,
                                   'link list' + (prefix.startsWith('help') ? ' help' : ''));
        link.onclick = function() { sendInput(prefix + ' ' + topic); };
        return link;
    })));

    appendBlock(wrapElements('div', elements, 'help'));
}

MessageHandler.prototype.describeRoom = function(brief, description, exits, contents)
{
    var elements = [makeTextElement('h1', brief)];
    Array.prototype.push.apply(elements, formatText(description));

    if (exits) {
        var exit_links = [makeTextElement('span', 'Exits:')].concat(
            exits.map(function (dir) {
                var link = makeTextElement('span', dir, 'link list');
                link.onclick = function() { sendInput(dir); };
                return link;
            }));
        elements.push(wrapElements('p', exit_links));
    }

    if (contents) {
        for (var i = 0; i < contents.length; ++i) {
            var [key, brief, pose] = contents[i];
            elements.push(this.formatEmote(key, brief, pose));
        }
    }

    appendBlock(wrapElements('div', elements));
}

MessageHandler.prototype.showList = function(header, items)
{
    appendBlock(wrapElements('div', [makeTextElement('p', header), makeList(items)]));
}

// Sets the current default target for attacks.
MessageHandler.prototype.setEnemyTarget = function(path)
{
    var properties = this.neighbors[path];

    // If there is a current target, change its highlight to be an enemy but not
    // the current target.
    if (this.enemy_target)
        setNeighborHighlight(this.enemy_target, 'enemy');

    this.enemy_target = path;
    setNeighborHighlight(this.enemy_target, 'enemy_target');
    this.showText('You begin attacking {0}.'.format(look(properties.brief)));
    this.enemies.add(path);
}

MessageHandler.prototype.clearEnemyTarget = function()
{
    if (this.enemy_target)
        setNeighborHighlight(this.enemy_target, 'hostile');
    this.enemy_target = undefined;
}

// Removes the entity from all target sets.
MessageHandler.prototype.removeTarget = function(id)
{
    this.enemies.delete(id);
    removeNeighborHighlight(id);
}

MessageHandler.prototype.showSay = function(prefix, text, is_chat)
{
    var elements = [];
    if (text.indexOf("\n\n") == -1) {
        var msg = prefix + ", &ldquo;" + text + "&rdquo;";
        elements.push(makeTextElement('p', msg));
    }
    else {
        elements.push(makeTextElement('p', prefix + ':'));
        elements = elements.concat(formatText(text, 'blockquote'));
    }
    appendBlock(wrapElements('div', elements), is_chat ? 'chat_text' : 'game_text');
}

MessageHandler.prototype.listMacros = function(macros)
{
    var elements = [];
    if (macros.length) {
        elements.push(makeTextElement('p', 'You have defined the following macros:'));
        elements.push(makeList(
            macros.map(function (macro) {
                var [name, command] = macro;
                return '{0}: {1}'.format(link(name), command);
            })));
    }
    else {
        elements.push(makeTextElement('p', "You haven't defined any macros."));
    }
    appendBlock(wrapElements('div', elements));
}

// A message received when one entity damages another entity.
MessageHandler.prototype.didAttack = function(actor, verb, amount, target, health, max_health)
{
    var actor_brief =
        (actor == this.player_path) ? 'You' : this.neighbors[actor].brief.capitalize();
    var target_brief =
        (target == this.player_path) ? 'you' : this.neighbors[target].brief;

    if  (actor == this.player_path)
        verb = makeVerbPlural(verb);

    var msg = '{0} {1} {2} for {3} damage!'.format(actor_brief, verb, target_brief, amount);
    this.showText(msg);

    if (target == this.player_path) {
        this.player_stats.health = health;
        this.player_stats.max_health = max_health;
        updateBar('player_health', health, max_health);
    }
    else {
        this.updateNeighbor({'path': target, 'health': health, 'max_health': max_health});
    }
}

// A message received when one entity kills another, and the victim is
// optionally replaced by a corpse.
MessageHandler.prototype.didKill = function(actor, target, corpse_properties)
{
    this.removeTarget(target);

    var actor_brief =
        (actor == this.player_path) ? 'You' : this.neighbors[actor].brief.capitalize();
    var target_brief =
        (target == this.player_path) ? 'you' : this.neighbors[target].brief;

    var msg = '{0} killed {1}!'.format(actor_brief, target_brief);
    this.showText(msg);

    this.replaceNeighbor(target, corpse_properties);
}

MessageHandler.prototype.echoCommand = function(msg)
{
    console.log(msg);
    this.showText('&raquo; ' + msg, 'cmd');
}

function sendInput(s)
{
    s = s.trim();
    if (s.length) {
        handler.echoCommand(s);
        session_send_message(s);
    }
}

function onUserInput(event)
{
    var obj = document.getElementById("input").children[0];
    command_history[command_pos] = obj.value;
    if (event.keyCode == 13) {  // enter
        if (obj.value.length > 0)
            sendInput(obj.value);
        obj.value = "";
        command_pos = (command_pos + 1) % command_history.length;
    }
    else if (event.keyCode == 38 || event.keyCode == 40) {  // up and down arrow
        var offset = (event.keyCode == 38 ? (command_history.length - 1) : 1);
        var new_pos = (command_pos + offset) % command_history.length;
        if (command_history[new_pos] != undefined) {
            obj.value = command_history[new_pos];
            command_pos = new_pos;
        }
    }
}

function session_on_open()
{
    // Nothing to do.
}

function session_on_close()
{
    handler.showText(
        'The server closed the connection. Please [return to the home page](index.html).',
        'error');
}

function session_send_message(msg)
{
    // TODO: Explicit UTF-8 encoding?
    ws.send(msg);
}

function session_on_message(event)
{
    console.log(event.data);
    var message = JSON.parse(event.data);
    handler[message[0]].apply(handler, message.slice(1));
}

function start()
{
    ws = new WebSocket('ws://' + location.host + '/game/session')
    ws.onopen = session_on_open;
    ws.onclose = session_on_close;
    ws.onmessage = session_on_message;

    var input = document.getElementById("input");
    input.focus();
    input.addEventListener("keydown", (event) => {
        onUserInput(event);
    });
}

document.addEventListener("DOMContentLoaded", function() { setTimeout(start, 0); });
