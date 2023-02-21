import stanza
from stanza.pipeline.core import DownloadMethod
from settings import LANG

# Download the English models for the neural pipeline
stanza.download(LANG)
# Just set up a dummy pipeline in order to download the resources
stanza.Pipeline(LANG, download_method=DownloadMethod.DOWNLOAD_RESOURCES)
