"""Provides all of the infrastructure to perform two-way encryption,
compression, and splitting on files with full MD5 support to ensure
operational integrity."""

import md5, time, yaml, sys, os, glob

###### Constants #############################################################

ENCRYPT    = "encrypt"
COMPRESS   = "compress"
SPLIT      = "split"
JOIN       = "join"
DECRYPT    = "decrypt"
UNCOMPRESS = "uncompress"

MANIFEST_FILE = "manifest.yml"
OUTPUT_FREQUENCY = 25
MAX_FILE_SIZE = 500 * (1024 * 1024) # 50 MiB
BLOCK_SIZE = 124000

###### Exceptions ############################################################

class NoPrivateKeyException(Exception):
    "If encryption is required but no secret key has been provided"
    pass

class NoPossibleFilesException(Exception):
    """If we are trying to figure out how to 'do the right thing based on
    filename, and there are no files we can operate on"""
    pass

class ZeroOutputException(Exception):
    """We hate to write out zero-byte files. This is almost always due to a
    logic error. Therefore just throw an exception if it looks like we're
    about to do this"""
    pass

class Md5ValidationFailure(Exception):
    """What we throw when the file has gone through reverse processing and
    the expected MD5 hash does not match what we thought we'd see."""
    def __init__(self, file_name):
        Exception.__init__(self)
        self.msg = "Md5 failed validation in step: %s" % file_name
    def __str__(self):
        return self.msg
    def __repr__(self):
        return self.msg

class NoOptionsException(Exception):
    "If no options are set, there is nothing to do"
    pass

###### MD5 Validation ########################################################

def strip_directory(file_name):
    """Opposite of get_base_path. We're interested only in 
    the filename without path information"""
    base_path, sep, base_file_name = file_name.rpartition(os.path.sep)
    if not sep:
        return file_name
    return base_file_name

class Md5Validation:
    """A mix-in class for all file-processor objects to provide for md5 
    computation and checking. Some classes operate on more than one file
    and so our md5 operations are stored in a dictionary."""
    def __init__(self, file_name, target_md5_data, logger):
        self.file_name = file_name
        self.target_md5_data = target_md5_data
        self.logger = logger
        self.computed_md5_data = {}

    def update(self, data):
        "Gives us data that we will use to compute our md5 information."
        #print "UPDATE: %s || file_name: %s || len(data) %d" % (self, self.file_name, len(data))
        key = strip_directory(self.file_name)
        if not key in self.computed_md5_data:
            self.computed_md5_data[key] = md5.new()
        if not data:
            return
        self.computed_md5_data[key].update(data)

    def dump_md5(self):
        output = {}
        for key in self.computed_md5_data:
            output[key] = self.computed_md5_data[key].hexdigest()
        return output

    def check_md5(self):
        if len(self.computed_md5_data) == 0:
            #self.logger.warning("No MD5 data was computed")
            return
        for key in self.computed_md5_data:
            if not key in self.target_md5_data.keys():
                self.logger.error( "=DIFFERENT==============================" )
                self.logger.error("COMPUTED: %s" %  self.computed_md5_data.keys() )
                self.logger.error("TARGET: %s" % self.target_md5_data.keys())
                self.logger.error( "========================================" )
                raise Md5ValidationFailure(key)
            computed_hex_digest = self.computed_md5_data[key].hexdigest()
            if computed_hex_digest != self.target_md5_data[key]:
                self.logger.error( "=DIFFERENT MD5 (%10s)==============" % self )
                self.logger.error( "COMPUTED MD5: %s" % computed_hex_digest )
                self.logger.error( "TARGET MD5: %s" %self.target_md5_data[key] )
                self.logger.error( "========================================" )
                raise Md5ValidationFailure(key)

###### File readers and writers ##############################################

class AbstractFileWriter:
    """Writes a file to disk, doing nothing to it. Does not record MD5 
    data because it is no different than the previous processor"""
    def __init__(self, file_name, logger):
        self.logger = logger
        self.file_name = file_name
        self.file_handle = None

    def __repr__(self):
        return "ABSTRACT_FILE_WRITER"

    def open(self):
        if not self.file_handle:
            self.file_handle = open(self.file_name, 'wb')

    def set_max_file_size(self, max_file_size):
        "used by the testing harness only. Does nothing on normal file writers"
        pass

    def write(self, data):
        self.open()
        self.file_handle.write(data)

    def close(self):
        if not self.file_handle:
            raise ZeroOutputException()
        self.file_handle.flush()
        self.file_handle.close()
        self.file_handle = None

class AbstractFileReader:
    "Base class for reading files from the disk"
    def __init__(self, file_name, logger):
        self.logger     = logger
        self.block_size = BLOCK_SIZE

        self.original_size = os.stat(file_name)[6]
        self.size_so_far = 0
        self.output = 0
        self.file_name = file_name
        self.file_handle = None
        self.open()

    def __repr__(self):
        return "ABSTRACT_FILE_READER"

    def open(self):
        """This is a clutch function to be able to read the source file and 
        to wait until it's not busy any longer"""
        keep_trying = 0
        while True:
            try:
                self.file_handle = open(self.file_name, 'rb')
                break
            except IOError, ioe:
                msg = "Unable to read %s (%d)"
                self.logger.info(msg % (self.file_name, keep_trying))
                keep_trying += 1
                time.sleep(2)
                if keep_trying > 20:
                    raise ioe

    def read(self):
        self.output  += 1
        data = self.file_handle.read(self.block_size)
        self.size_so_far += len(data)
        if self.output > OUTPUT_FREQUENCY:
            done = (100.0*(float(self.size_so_far)/float(self.original_size)))
            self.logger.info("%3.0f percent done..." % done)
            self.output = 0
        return data

    def set_block_size(self, block_size):
        "used by the testing harness only"
        self.block_size = block_size

    def close(self):
        self.file_handle.close()

class FileReader(AbstractFileReader, Md5Validation):
    "This will read a file from the disk, doing nothing to it."
    def __init__(self, file_name, target_md5_data, logger):
        AbstractFileReader.__init__(self, file_name, logger)
        Md5Validation.__init__(self, file_name, target_md5_data, logger)

    def __repr__(self):
        return "FILE_READER"

    def read(self):
        data = AbstractFileReader.read(self)
        Md5Validation.update(self, data)
        return data

class JoinReader(AbstractFileReader, Md5Validation):
    """Reads a series of files from the disk, all ending in .part?, for 
    example, foo.part0, foo.part1, foo.part2... and join them back up"""
    def __init__(self, base_file_name, target_md5_data, logger):
        Md5Validation.__init__(self, base_file_name, target_md5_data, logger)
        self.file_number = 0
        self.base_file_name = base_file_name
        self.get_file_name()
        AbstractFileReader.__init__(self, self.file_name, logger)

    def __repr__(self):
        return "JOIN_READER"

    def get_file_name(self):
        self.file_name = "%s.part%d" % (self.base_file_name, self.file_number)

    def read(self):
        """Provides a read function similar to any file, but in this case
        will switch to a different file if the current file has run out."""
        data = AbstractFileReader.read(self)
        Md5Validation.update(self, data)
        while not data:
            self.file_number += 1
            self.get_file_name()
            if not os.path.isfile(self.file_name):
                return ''
            self.open()
            data = AbstractFileReader.read(self)
            Md5Validation.update(self, data)
        return data

class NullWriter(AbstractFileWriter, Md5Validation):
    """Does not write. This is used when the source and destination files are 
    the same thing, in which case we are merely verifying md5 hash values"""
    def __init__(self, logger):
        target_md5_data = {}
        AbstractFileWriter.__init__(self, '', None, logger)
        Md5Validation.__init__(self, '', target_md5_data, logger)
    def write(self, data):
        pass
    def open(self):
        pass
    def close(self):
        pass
    def __repr__(self):
        return "NULL_WRITER"

class FileWriter(AbstractFileWriter, Md5Validation):
    """Writes a file to disk, doing nothing to it. Does not record MD5 
    data because it is no different than the previous processor"""
    def __init__(self, base_file_name, target_md5_data, logger):
        target_md5_data = {}
        AbstractFileWriter.__init__(self, base_file_name, logger)
        Md5Validation.__init__(self, base_file_name, target_md5_data, logger)
    def __repr__(self):
        return "FILE_WRITER"


class SplitWriter(AbstractFileWriter, Md5Validation):
    "This class splits files into parts as it writes."
    def __init__(self, base_file_name, logger):
        target_md5_data = {}
        AbstractFileWriter.__init__(self, base_file_name, logger)
        Md5Validation.__init__(self, base_file_name, target_md5_data, logger)
        self.base_file_name = base_file_name
        self.max_file_size = MAX_FILE_SIZE
        self.file_size = 0
        self.file_number = 0

    def __repr__(self):
        return "SPLIT_WRITER"

    def open(self):
        if not self.file_handle:
            self.file_name = "%s.part%d" % (self.base_file_name, self.file_number)
            self.file_handle = open(self.file_name, 'wb')

    def set_max_file_size(self, max_file_size):
        "used by the testing harness only"
        self.max_file_size = max_file_size

    def write(self, data):
        self.open()
        self.file_size += len(data)

        if self.file_size > self.max_file_size:
            self.file_size = 0
            self.file_number += 1
            self.close()
            self.open()
        Md5Validation.update(self, data)
        self.file_handle.write(data)

###### Stream processors #####################################################

class AbstractProcessor:
    """Abstract class that can process a stream of data. These are intended
    to be chained together, the output from one procesor used as the input
    for the next, etc."""
    def __init__(self, logger):
        self.logger = logger
        self.block_size = BLOCK_SIZE
        self.read_buffer = ''
    def __repr__(self):
        return "PLUGGABLE_PROCESSOR_BASE_CLASS"
    def set_block_size(self, block_size):
        "used by the testing harness only"
        self.block_size = block_size  + (16 - block_size % 16)
    def write(self, data):
        """This allows us to process new data. What we return is the
        processed data for the next Pluggable Processor to act upon"""
        self.read_buffer = data
    def read(self):
        data = self.read_buffer
        self.read_buffer = ''
        return data
    def flush(self):
        """This gives us an opportunity to flush out any data we may 
        have saved with the idea that we can optimize our output later on"""
        return ''
    def suffix(self):
        "This influences the name of the written file"
        return ""

class Compressor(AbstractProcessor, Md5Validation):
    "provides bz2 compression"
    def __init__(self, base_file_name, logger):
        import bz2
        AbstractProcessor.__init__(self, logger)
        target_md5_data = {}
        file_name = base_file_name + self.suffix()
        Md5Validation.__init__(self, file_name, target_md5_data, logger)
        self.compressor = bz2.BZ2Compressor()
        self.read_buffer = ''
    def __repr__(self):
        return "COMPRESSOR"
    def write(self, data):
        data = self.compressor.compress(data)
        Md5Validation.update(self, data)
        self.read_buffer = data
    def flush(self):
        final_data = self.compressor.flush()
        Md5Validation.update(self, final_data)
        return final_data
    def suffix(self):
        return ".bz2"

class UnCompressor(AbstractProcessor, Md5Validation):
    "Reverse process of Compressor"
    def __init__(self, base_file_name, target_md5_data, logger):
        import bz2
        AbstractProcessor.__init__(self, logger)
        file_name = base_file_name + self.suffix()
        Md5Validation.__init__(self, file_name, target_md5_data, logger)
        self.decompressor = bz2.BZ2Decompressor()
    def __repr__(self):
        return "UNCOMPRESSOR"
    def write(self, data):
        Md5Validation.update(self, data)
        self.read_buffer = self.decompressor.decompress(data)
    def flush(self):
        return ''
    def suffix(self):
        return ".bz2"

class Encryptor(AbstractProcessor, Md5Validation):
    """Provides AES encryption. Everything the compressor class 
    does automatically, we have to do ourselves."""
    def __init__(self, base_file_name, private_key, logger):
        AbstractProcessor.__init__(self, logger)
        target_md5_data = {}
        file_name = base_file_name + self.suffix()
        Md5Validation.__init__(self, file_name, target_md5_data, logger)
        private_key += '=' * (16 - len(private_key) % 16)
        from Crypto.Cipher import AES
        self.encrypter = AES.new(private_key, AES.MODE_ECB)
        self.write_buffer = ''
    def __repr__(self):
        return "ENCRYPTOR"
    def write(self, data):
        "Can only process chunks of the right size"
        self.write_buffer += data
        if len(self.write_buffer) > self.block_size:
            self.read_buffer += self.encrypter.encrypt(self.write_buffer[:self.block_size])
            Md5Validation.update(self, self.read_buffer)
            self.write_buffer = self.write_buffer[self.block_size:]
    def flush(self):
        """AES requires blocks of 16 bytes, so we'll pad it
        and then mark the last byte with the length of 
        the pad for proper decryption"""
        pad_length = 15 - (len(self.write_buffer) % 16)
        pad = 'X'*pad_length
        pad += chr(pad_length)
        self.write_buffer += pad
        final_data = self.encrypter.encrypt(self.write_buffer)
        Md5Validation.update(self, final_data)
        return final_data
    def suffix(self):
        return ".enc"

class Decryptor(AbstractProcessor, Md5Validation):
    "Reverse of the Encryptor class"
    def __init__(self, base_file_name, private_key, target_md5_data, logger):
        AbstractProcessor.__init__(self, logger)
        file_name = base_file_name + self.suffix()
        Md5Validation.__init__(self, file_name, target_md5_data, logger)
        private_key += '=' * (16 - len(private_key) % 16)

        self.logger = logger
        from Crypto.Cipher import AES
        self.decryptor = AES.new(private_key, AES.MODE_ECB)
        self.write_buffer = ''
    def __repr__(self):
        return "DECRYPTOR"
    def write(self, data):
        """We validate the MD5 hash prior to decryption, because we 
        are comparing it to the output from the encryptor."""
        self.write_buffer += data
        if len(self.write_buffer) > self.block_size:
            self.read_buffer = self.decryptor.decrypt(self.write_buffer[:self.block_size])
            self.write_buffer = self.write_buffer[self.block_size:]
        Md5Validation.update(self, self.read_buffer)
    def flush(self):
        "Need to determine how much of the final data is pad and strip it"
        last_data_hunk = self.decryptor.decrypt(self.write_buffer)
        pad_size = ord(last_data_hunk[-1])
        if pad_size > 32:
            msg = "Invalid PAD size. Data is corrupt."
            self.logger.warning(msg)
            raise IOError
        good_data_length = len(last_data_hunk) - pad_size - 1
        
        final_data = last_data_hunk[:good_data_length]
        Md5Validation.update(self, final_data)
        return final_data

###### Pluggable processors ##################################################

def process_all(reader, processors, writer, priority):
    """Going through all the configured processors and writing 
    out a processed file"""
    new_data = reader.read()
    while new_data:
        time.sleep(0.01 * priority)
        for processor in processors:
            processor.write(new_data)
            new_data = processor.read()
        writer.write(new_data)
        new_data = reader.read()

    for index in range(0, len(processors)):
        processor = processors[index]
        final_data = processor.flush()
        if final_data:
            for processor in processors[index+1:]:
                processor.write(final_data)
                final_data = processor.read()
            writer.write(final_data)

    writer.close()
    reader.close()

def get_base_path(file_name):
    """return the directory that file_name is in. If it's not in a 
    separate directory, return nothing."""
    base_path, middle, end = file_name.rpartition(os.path.sep)
    if not middle:
        return ''
    return base_path

def get_manifest_data(file_name):
    manifest_path = os.path.join(get_base_path(file_name), MANIFEST_FILE)
    md5_dict = {}
    if os.path.isfile(manifest_path):
        md5_string = open(manifest_path, 'r').read()
        md5_dict = yaml.load(md5_string)
        if type(md5_dict) != type({}):
            md5_dict = {}
    return md5_dict

def get_suffix_file_name(file_name, processors):
    "Returns the name of the file we're currently working with"
    suffix_file_name = file_name
    for processor in processors:
        suffix_file_name += processor.suffix()
    return suffix_file_name

class ForwardPluggableFileProcessor:
    """This is for performing the 'forward' actions, i.e. encryption, 
    compression, and/or splitting. Takes options for what to do and
    writes out a manifest.yml file for md5 information."""

    def __init__(self, file_name, options,
                 logger, private_key=''):
        self.file_name         = file_name
        self.logger            = logger
        self.processors        = []
        if not options:
            raise NoOptionsException
            
        creation_file_name = self.file_name
        if COMPRESS in options:
            self.processors.append( Compressor(creation_file_name, self.logger) )
            creation_file_name += ".bz2"
        else:
            self.logger.warning("Not configured to compress.")

        if ENCRYPT in options:
            if not private_key:
                raise NoPrivateKeyException()
            try:
                encryptor = Encryptor(creation_file_name, private_key, self.logger)
                creation_file_name += ".enc"
                self.processors.append(encryptor)
            except ImportError:
                msg = "Encryption libraries are not installed. Not encrypting."
                self.logger.warning(msg)
        else:
            self.logger.warning("Not configured to encrypt.")

        target_md5_data = {}
        suffix_file_name = get_suffix_file_name(self.file_name, self.processors)
        self.reader = FileReader(self.file_name, target_md5_data, self.logger)
        if SPLIT in options:
            self.writer = SplitWriter(creation_file_name, self.logger)
        else:
            self.writer = FileWriter(creation_file_name, target_md5_data,
                                     self.logger)

    def set_block_size(self, block_size):
        "used by the testing harness only"
        self.reader.set_block_size(block_size)
        for processor in self.processors:
            processor.set_block_size(block_size)

    def set_max_file_size(self, max_file_size):
        "used by the testing harness only"
        self.writer.set_max_file_size(max_file_size)

    def dump_md5_sums(self):
        "Returns a dictionary of all md5sums collected"
        md5_dict = {}
        md5_dict.update(self.reader.dump_md5())
        md5_dict.update(self.writer.dump_md5())
        for processor in self.processors:
            md5_dict.update(processor.dump_md5())
        return md5_dict

    def create_manifest(self):
        old_md5_data = get_manifest_data(self.file_name)
        md5_data = self.dump_md5_sums()
        md5_data.update(old_md5_data)
        yaml_string = yaml.dump(md5_data)
        manifest_path = os.path.join(get_base_path(self.file_name), MANIFEST_FILE)
        open(manifest_path, 'w').write(yaml_string)

    def process_all(self, priority=0):
        process_all(self.reader, self.processors, self.writer, priority)
        self.create_manifest()

class ReversePluggableFileProcessor:
    """For performing 'reverse' operations to undo the operations in
    ForwardPluggableFileProcessor: decrypt, uncompress, and join. This
    will look at a directory and 'do the right thing' with the files that
    are there, based on extension, and will validate against md5 hash
    information in the manifest.yml file"""
    def __init__(self, file_name, logger, private_key = ''):
        self.file_name         = file_name
        self.logger            = logger
        self.processors        = []

        target_md5_data = get_manifest_data(self.file_name)

        options = self.determine_options()
        source_file_name = self.source_file_name(options)
        if not options:
            self.writer = NullWriter(self.logger)
        else:
            self.writer = FileWriter(self.file_name, target_md5_data,
                                     self.logger)

        if JOIN in options:
            self.reader = JoinReader(source_file_name, target_md5_data,
                                     self.logger)
        else:
            self.reader = FileReader(source_file_name, target_md5_data,
                                     self.logger)
        if DECRYPT in options:
            if not private_key:
                raise NoPrivateKeyException()
            source_file_name = source_file_name.partition('.enc')[0]
            decryptor = Decryptor(source_file_name, private_key, 
                                  target_md5_data, self.logger)
            self.processors.append(decryptor)
        if UNCOMPRESS in options:
            source_file_name = source_file_name.partition('.bz2')[0]
            uncompressor = UnCompressor(source_file_name, target_md5_data,
                                        self.logger)
            self.processors.append(uncompressor)

    def determine_options(self):
        """Based on files in a directory, determine what options we should 
        do and what file(s) we should operate on."""
        if os.path.isfile(self.file_name):
            return []
        possible_files = glob.glob(self.file_name+"*")
        suffix_set = set()
        for fname in possible_files:
            new_suffix_list = fname.replace(self.file_name, '').split('.')
            suffix_set = suffix_set.union(new_suffix_list)
        options = []

        if "part0" in suffix_set:
            options.append(JOIN)
        if "enc" in suffix_set:
            options.append(DECRYPT)
        if "bz2" in suffix_set:
            options.append(UNCOMPRESS)
        return options

    def source_file_name(self, options):
        "Identify the name of the file we're working on"
        source_file_name = self.file_name
        if UNCOMPRESS in options:
            source_file_name += ".bz2"
        if DECRYPT in options:
            source_file_name += ".enc"
        return source_file_name

    def process_all(self, priority = 0):
        process_all(self.reader, self.processors, self.writer, priority)
        self.check_md5_sums()
        
    def check_md5_sums(self):
        manifest_path = os.path.join(get_base_path(self.file_name), MANIFEST_FILE)
        if os.path.isfile(manifest_path):
            self.reader.check_md5()
            for processor in self.processors:
                processor.check_md5()
            self.writer.check_md5()
        else:
            msg = "%s doesn't exist. Not checking MD5" % manifest_path
            self.logger.warning(msg)
