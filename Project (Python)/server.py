import asyncio
import argparse
import re
import time
import aiohttp
import json
import logging
import os

# if putty aborts, must use netstat -tlnp, then kill -9 pid to close port

API_Key = "YOUR_API_KEY"
server_IDs = ["Hill", "Jaquez", "Smith", "Campbell", "Singleton"]
server_tree = {
    "Hill": ["Jaquez", "Smith"],
    "Jaquez": ["Hill", "Singleton"],
    "Smith": ["Hill", "Singleton", "Campbell"],
    "Singleton": ["Campbell", "Jaquez", "Smith"],
    "Campbell": ["Singleton", "Smith"]
}
port_assignments = {
    "Hill": 11950,
    "Jaquez": 11951,
    "Smith": 11952,
    "Singleton": 11953,
    "Campbell": 11954
}

### Server class ###
class Server:
    # TODO: the port argument is not used
    def __init__(self, name, ip='127.0.0.1', port=11951, message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port_assignments[name]
        self.message_max_length = int(message_max_length)
        # a set to remember flood messages already received
        self.flood_msgs = set()
        # client name maps to dictionary of client attributes ("lat": latitude, etc)
        self.clients = {}

    async def handle_echo(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        curr_time = time.time()
        message = data.decode()
        addr = writer.get_extra_info('peername')
        print("{} received {} from {}".format(self.name, message, addr))
        project_log.write(self.name + " received " + message + " from " + str(addr) + '\n')

        #process the message and send back appropriate message
        cmd = message.split()[0] if message else ""
        parsed_msg = message.split()
            
        if cmd == "IAMAT":
            sendback_message = await self.IAMAT(parsed_msg, curr_time)
        elif cmd == "WHATSAT":
            sendback_message = await self.WHATSAT(parsed_msg)
        elif cmd == "AT":
            await self.AT(message)
            sendback_message = None
        else:
            # handle invalid query
            sendback_message = "? " + message
        
        if sendback_message is not None:
            print("{} send: {}".format(self.name, sendback_message))
            project_log.write(self.name + " send: " + sendback_message + '\n')
            writer.write(sendback_message.encode())
            await writer.drain()

        print("close the client socket")
        writer.close()


    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)
        # Serve requests until Ctrl+C is pressed
        print(f'serving on {server.sockets[0].getsockname()}')
        project_log.write(self.name + " is serving on " + str({server.sockets[0].getsockname()}) + '\n')
        async with server:
            await server.serve_forever()

        # Close the server
        project_log.write(self.name + " is closing...\n")
        server.close()
       
    # server to server communication and updating of client attributes
    async def AT(self, flood_msg): # take in flood_msg
        if flood_msg not in self.flood_msgs:
            self.flood_msgs.add(flood_msg)
            parsed_msg = flood_msg.split()
            self.update_client(
                origin_server=parsed_msg[1],
                client_name=parsed_msg[3],
                lat_lon_str=parsed_msg[4],
                client_time=parsed_msg[5],
                time_diff_str=parsed_msg[2]
            )
            project_log.write("In " + self.name + ", updating client attributes for " + parsed_msg[3] + ", located at " + parsed_msg[1] + '\n')
            await self.flood_it(flood_msg)
        
    # for i in 11950 11951 11952 11953 11954 ; do kill $(/usr/sbin/lsof -ti:$i); done
    # flooding algorithm for sending client updates to servers in the server_tree
    async def flood_it(self, flood_msg):
        parsed_msg = flood_msg.split()
        origin_server = parsed_msg[1]
        project_log.write("Flooding all servers connected to " + self.name + " in server_tree\n")
        for server in set(server_tree[self.name]) - {origin_server}:
            port = port_assignments[server]
            try:
                project_log.write("Connecting to " + server + '\n')

                reader, writer = await asyncio.open_connection("127.0.0.1", port)

                project_log.write("Writing " + flood_msg + " from " + self.name + " to " + server + '\n')
                writer.write(flood_msg.encode())
                await writer.drain()
            except:
                project_log.write(server + " is down. Cannot connect\n")
                continue

    async def WHATSAT(self, parsed_msg):
        if len(parsed_msg) != 4:
            return "? " + " ".join(parsed_msg)
        if float(parsed_msg[2]) > 50 or float(parsed_msg[2]) < 0:
            return "? " + " ".join(parsed_msg)
        if int(parsed_msg[3]) > 20 or int(parsed_msg[3]) < 0:
            return "? " + " ".join(parsed_msg)
        
        client_id = parsed_msg[1]
        if client_id not in self.clients:
            return "? " + " ".join(parsed_msg)
        client_specs = self.clients[client_id]
           
        sendback_message = f'AT {client_specs["server"]} {client_specs["time_diff"]} {client_id} {client_specs["lat"]}{client_specs["lon"]} {client_specs["time"]}'
        
        # TODO: we don't need the global keyword since API_Key is already declared at a global scope
        # Querying the Google Places API
        formatted_rec_loc = client_specs["lat"] + "," + client_specs["lon"]
        radius = float(parsed_msg[2])

        url_w_params = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s&radius=%d&key=%s' % (formatted_rec_loc, radius, API_Key)

        project_log.write(self.name + " is sending an HTTP request to " + url_w_params + '\n')
        async with aiohttp.ClientSession() as session:
            async with session.get(url_w_params) as resp:
                response = await resp.json()

        # limiting the results and formatting
        response['results'] = response['results'][:(int(parsed_msg[3]))]
        response = json.dumps(response, indent=4)
        project_log.write(self.name + " received " + response + " from Google API request\n")

        sendback_message += "\n" + response + "\n\n"
        return sendback_message
            
    async def IAMAT(self, parsed_msg, curr_time):
        if len(parsed_msg) != 4:
            return "? " + " ".join(parsed_msg)

        time_difference = curr_time - float(parsed_msg[3])
        if time_difference > 0:
            time_difference_str = "+" + str(time_difference)
        else:
            time_difference_str = str(time_difference)

        server_name = self.name
        self.update_client(
            origin_server=server_name,
            client_name=parsed_msg[1],
            lat_lon_str=parsed_msg[2],
            client_time=parsed_msg[3],
            time_diff_str=time_difference_str
        )
        project_log.write("Updating client attributes for " + parsed_msg[1] + " at " + server_name + '\n')

        from_sent = " ".join(parsed_msg[1:])
        sendback_message = "AT " + server_name + " " + time_difference_str + " " + from_sent
        project_log.write("Flooding server herd with updated client attributes, beginning at " + server_name + '\n')

        # send the client's updated attributes to the connected servers
        self.flood_msgs.add(sendback_message)
        await self.flood_it(sendback_message)
        return sendback_message

    def update_client(self, origin_server, client_name, lat_lon_str, client_time, time_diff_str):
        # update client attributes in clients
        latitude, longitude = get_lat_lon(lat_lon_str)
        client_attributes = {
            "name": client_name,
            "lat": latitude,
            "lon": longitude,
            "time": client_time,
            "time_diff": time_diff_str,
            "server": origin_server
        }
        self.clients[client_name] = client_attributes
       
    
def get_lat_lon(lat_lon):
    if '+' in lat_lon[1:]:
        splitted = lat_lon[1:].split('+')
        splitted[1] = '+' + splitted[1]
    else:
        splitted = lat_lon[1:].split('-')
        splitted[1] = '-' + splitted[1]
    splitted[0] = lat_lon[0] + splitted[0]
    return splitted



def main():
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    # TODO: let's make this project_log a member of the server class
    global project_log
    project_log = open(args.server_name + ".log", "a")

    # TODO: maybe create a helper function to do the project_log.write so that we don't risk forgetting to append the newline character...
    project_log.write("Beginning of program run\n")

    print("Hello, welcome to server {}".format(args.server_name))

    server = Server(args.server_name)
    project_log.write("Server " + args.server_name + " has been initialized and opened\n")

    try:
        project_log.write("Event loop has been initialized\n")
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()
